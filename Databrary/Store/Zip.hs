{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Databrary.Store.Zip
  ( ZipEntryContent(..)
  , ZipEntry(..)
  , blankZipEntry
  , streamZip
  , writeZipFile
  , fileZipEntry
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.RWS.Strict (RWST, runRWST, ask, local, tell)
import Control.Monad.State.Class (MonadState, get, modify')
import Control.Monad.State.Strict (runStateT)
import Data.Bits (bit, shiftL, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Extra as B (defaultChunkSize)
import qualified Data.ByteString.Lazy as BSL
import Data.Digest.CRC32 (crc32, crc32Update)
import Data.Maybe (isJust, fromMaybe, fromJust)
import Data.Monoid (Monoid, mempty, (<>))
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (UTCTime(..), getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime (TimeOfDay(..), timeToTimeOfDay)
import Data.Word (Word32, Word64)
import System.IO (withBinaryFile, IOMode(ReadMode, WriteMode))
import System.IO.Error (mkIOError, eofErrorType)
import System.Posix.Directory.Foreign (dtDir, dtReg)
import System.Posix.Directory.Traversals (getDirectoryContents)
import System.Posix.Files.ByteString (isDirectory, modificationTimeHiRes, fileSize)

import Databrary.Ops
import Databrary.Files

data ZipEntryContent
  = ZipDirectory [ZipEntry]
  | ZipEntryPure BSL.ByteString
  | ZipEntryFile RawFilePath
  deriving (Show, Eq)

type ZipPath = BS.ByteString

data ZipEntry = ZipEntry
  { zipEntryName :: ZipPath
  , zipEntryTime :: Maybe UTCTime
  , zipEntryCRC32 :: Maybe Word32
  , zipEntrySize :: Maybe Word64
  , zipEntryComment :: BS.ByteString
  , zipEntryContent :: ZipEntryContent
  } deriving (Show, Eq)

blankZipEntry :: ZipEntry
blankZipEntry = ZipEntry
  { zipEntryName = BS.empty
  , zipEntryTime = Nothing
  , zipEntryCRC32 = Nothing
  , zipEntrySize = Nothing
  , zipEntryComment = BS.empty
  , zipEntryContent = ZipEntryPure BSL.empty
  }

getEntryCRC32 :: ZipEntry -> Maybe Word32
getEntryCRC32 ZipEntry{ zipEntryContent = ZipDirectory _ } = return 0
getEntryCRC32 ZipEntry{ zipEntryCRC32 = Just c } = return c
getEntryCRC32 ZipEntry{ zipEntryContent = ZipEntryPure b } = return $ crc32 b
getEntryCRC32 ZipEntry{ zipEntrySize = Just 0 } = return 0
getEntryCRC32 _ = Nothing

getEntrySize :: ZipEntry -> IO Word64
getEntrySize ZipEntry{ zipEntryContent = ZipDirectory _ } = return 0
getEntrySize ZipEntry{ zipEntrySize = Just s } = return s
getEntrySize ZipEntry{ zipEntryContent = ZipEntryPure b } = return $ fromIntegral $ BSL.length b
getEntrySize ZipEntry{ zipEntryContent = ZipEntryFile f } = maybe 0 (fromIntegral . fst) <$> fileInfo f

zip64Size :: Word64
zip64Size = 0xffffffff

if64 :: Word64 -> a -> a -> a
if64 s t e = if s >= zip64Size then t else e

zip64Ext :: B.Builder
zip64Ext = B.word16LE 1

zipSize :: Word64 -> B.Builder
zipSize = B.word32LE . fromIntegral . min zip64Size

zipTime :: UTCTime -> B.Builder
zipTime UTCTime{..} = B.word16LE time <> B.word16LE date where
  time = fromIntegral todHour `shiftL` 11 .|. fromIntegral todMin `shiftL` 5 .|. ceiling todSec `div` 2
  TimeOfDay{..} = timeToTimeOfDay utctDayTime
  date = fromIntegral (year - 1980) `shiftL` 9 .|. fromIntegral month `shiftL` 5 .|. fromIntegral day
  (year, month, day) = toGregorian utctDay

zipVersion :: Bool -> B.Builder
zipVersion False = B.word16LE 20
zipVersion True = B.word16LE 45

zipFlags :: Bool -> B.Builder
zipFlags False = B.word16LE $ bit 3
zipFlags True = B.word16LE 0

twice :: Monoid m => m -> m
twice m = m <> m

data ZipCEntry = ZipCEntry
  { zipCEntryPath :: !ZipPath
  , zipCEntryTime :: !UTCTime
  , zipCEntryKnown :: !Bool
  , zipCEntryCRC32 :: !Word32
  , zipCEntrySize :: !Word64
  , zipCEntryOffset :: !Word64
  , zipCEntry :: !ZipEntry
  }

streamZip :: [ZipEntry] -> BS.ByteString -> (B.Builder -> IO ()) -> IO ()
streamZip entries comment write = do
  t <- getCurrentTime
  ((), off, centries) <- runRWST (streamZipEntries entries) (BS.empty, t) 0
  (z64s, csize) <- runStateT (mapM streamZipCEntry centries) 0
  when (or z64s) $ write
    $ B.word32LE 0x06064b50
    <> B.word64LE 44 -- length of this record
    <> B.word16LE 63
    <> zipVersion True
    <> B.word32LE 0 -- disk
    <> B.word32LE 0 -- central disk
    <> twice (B.word64LE $ fromIntegral $ length centries)
    <> B.word64LE csize
    <> B.word64LE off
    <> B.word32LE 0x07064b50 -- locator:
    <> B.word32LE 0 -- central disk
    <> B.word64LE (off + csize)
    <> B.word32LE 1 -- total disks
  write $ B.word32LE 0x06054b50
    <> B.word16LE 0 -- disk
    <> B.word16LE 0 -- central disk
    <> twice (B.word16LE $ fromIntegral $ min 0xffff $ length centries)
    <> zipSize csize
    <> zipSize off
    <> B.word16LE (fromIntegral $ BS.length comment)
    <> B.byteString comment
  where
  slash (ZipDirectory _) p = BSC.snoc p '/'
  slash _ = id
  send :: (MonadState Word64 m, MonadIO m) => Int -> B.Builder -> m ()
  send l b = do
    modify' (fromIntegral l +)
    liftIO $ write b
  streamZipEntries = mapM_ streamZipEntry
  streamZipEntry :: ZipEntry -> RWST (BS.ByteString, UTCTime) [ZipCEntry] Word64 IO ()
  streamZipEntry z@ZipEntry{..} = do
    (path', time') <- ask
    off <- get
    let crc = getEntryCRC32 z
        known = isJust crc
        path = slash zipEntryContent $ path' <> zipEntryName
        time = fromMaybe time' zipEntryTime
        central c s = tell [ZipCEntry
          { zipCEntryPath = path
          , zipCEntryTime = time
          , zipCEntryKnown = known
          , zipCEntryCRC32 = c
          , zipCEntrySize = s
          , zipCEntryOffset = off
          , zipCEntry = z
          }]
    size <- if known then liftIO $ getEntrySize z else return 0
    let el = if64 size 20 0
    send (30 + BS.length path + fromIntegral el)
      $ B.word32LE 0x04034b50
      <> zipVersion (size >= zip64Size || off >= zip64Size)
      <> zipFlags known
      <> B.word16LE 0 -- compression
      <> zipTime time
      <> B.word32LE (fromMaybe 0 crc)
      <> twice (zipSize size)
      <> B.word16LE (fromIntegral $ BS.length path)
      <> B.word16LE el
      <> B.byteString path
      <> (if64 size (zip64Ext <> B.word16LE 16 <> twice (B.word64LE size)) mempty)
    case zipEntryContent of
      ZipDirectory l -> do
        central (fromJust crc) size
        local (\_ -> (path, time)) $ streamZipEntries l
      ZipEntryPure b -> do
        central (fromJust crc) size
        send (fromIntegral $ BSL.length b) (B.lazyByteString b)
      ZipEntryFile f
        | known -> do
          let run 0 _ = return ()
              run s h = do
                b <- BS.hGetSome h $ fromIntegral $ fromIntegral B.defaultChunkSize `min` s
                if BS.null b
                  then ioError $ mkIOError eofErrorType "ZipEntryFile" (Just h) (Just $ toFilePath f)
                  else do
                    write $ B.byteString b
                    run (s - fromIntegral (BS.length b)) h
          liftIO $ withBinaryFile (toFilePath f) ReadMode $ run size
          modify' (size +)
          central (fromJust crc) size
        | otherwise -> do
          let run r@(c, s) h = do
                b <- BS.hGetSome h B.defaultChunkSize
                if BS.null b
                  then return r
                  else do
                    write $ B.byteString b
                    run (crc32Update c b, s + fromIntegral (BS.length b)) h
          (c, s) <- liftIO $ withBinaryFile (toFilePath f) ReadMode $ run (0, 0)
          modify' (s +)
          let z64 = s >= zip64Size
          send (if z64 then 24 else 16)
            $ B.word32LE 0x08074b50
            <> B.word32LE c
            <> twice ((if z64 -- this seems weird but it's what openjdk's ZipOutputStream does
              then B.word64LE else B.word32LE . fromIntegral) s)
          central c s
  streamZipCEntry ZipCEntry{..} = do
    let z64 = zipCEntrySize >= zip64Size || zipCEntryOffset >= zip64Size
        e64 = if64 zipCEntrySize 16 0 + if64 zipCEntryOffset 8 0
        el = if z64 then 4 + e64 else 0
    send (46 + BS.length zipCEntryPath + fromIntegral el + BS.length (zipEntryComment zipCEntry))
      $ B.word32LE 0x02014b50
      <> B.word16LE 63 -- version
      <> zipVersion z64
      <> zipFlags zipCEntryKnown
      <> B.word16LE 0 -- compression
      <> zipTime zipCEntryTime
      <> B.word32LE zipCEntryCRC32
      <> twice (zipSize zipCEntrySize)
      <> B.word16LE (fromIntegral $ BS.length zipCEntryPath)
      <> B.word16LE el
      <> B.word16LE (fromIntegral $ BS.length $ zipEntryComment zipCEntry)
      <> B.word16LE 0 -- disk number
      <> B.word16LE 0 -- if text then bit 1
      <> B.word32LE 0
      <> zipSize zipCEntryOffset
      <> B.byteString zipCEntryPath
      <> (if z64
        then zip64Ext <> B.word16LE e64 <> if64 zipCEntrySize (twice (B.word64LE zipCEntrySize)) mempty <> if64 zipCEntryOffset (B.word64LE zipCEntryOffset) mempty
        else mempty)
      <> B.byteString (zipEntryComment zipCEntry)
    return z64

writeZipFile :: FilePath -> [ZipEntry] -> BS.ByteString -> IO ()
writeZipFile f e c =
  withBinaryFile f WriteMode $
    streamZip e c . B.hPutBuilder

fileZipEntry :: RawFilePath -> IO ZipEntry
fileZipEntry src = do
  s <- getFileStatus src
  let t = posixSecondsToUTCTime $ modificationTimeHiRes s
  if isDirectory s
    then dir src src (Just t)
    else return $ file' src src (fileSize s) t
  where
  dir d n t = do
    l <- getDirectoryContents d
    c <- mapMaybeM (ent d) l
    return ZipEntry
      { zipEntryName = n
      , zipEntryTime = t
      , zipEntryCRC32 = Nothing
      , zipEntrySize = Nothing
      , zipEntryComment = BS.empty
      , zipEntryContent = ZipDirectory c
      }
  ent d (t,n)
    | n == "." || n == ".." = return Nothing
    | t == dtDir = Just <$> dir (d </> n) n Nothing
    | t == dtReg = Just <$> file (d </> n) n
    | otherwise = return Nothing
  file f n = do
    Just (s, t) <- fileInfo f
    return $ file' f n s t
  file' f n s t = ZipEntry
    { zipEntryName = n
    , zipEntryTime = Just t
    , zipEntryCRC32 = Nothing
    , zipEntrySize = Just $ fromIntegral s
    , zipEntryComment = BS.empty
    , zipEntryContent = ZipEntryFile f
    }
