{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Databrary.Store.Transcode
  ( startTranscode
  , forkTranscode
  , stopTranscode
  , collectTranscode
  , transcodeEnabled
  ) where

import Control.Concurrent (ThreadId)
import Control.Monad (guard, unless, void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid ((<>))
import System.Exit (ExitCode(..))
import Text.Read (readMaybe)

import Databrary.Ops
import Databrary.Has (view, peek, peeks, focusIO)
import Databrary.Service.Log
import Databrary.HTTP.Route (routeURL)
import Databrary.Model.Segment
import Databrary.Model.Asset
import Databrary.Model.AssetSlot
import Databrary.Model.Format
import Databrary.Model.Transcode
import Databrary.Files
import Databrary.Store.Types
import Databrary.Store.Temp
import Databrary.Store.Asset
import Databrary.Store.Transcoder
import Databrary.Store.AV
import Databrary.Store.Probe
import Databrary.Action.Types

import {-# SOURCE #-} Databrary.Controller.Transcode

ctlTranscode :: Transcode -> TranscodeArgs -> ActionM (ExitCode, String, String)
ctlTranscode tc args = do
  t <- peek
  Just ctl <- peeks storageTranscoder
  let args'
        = "-i" : show (transcodeId tc)
        : "-f" : BSC.unpack (head (formatExtension (assetFormat (transcodeAsset tc))))
        : args
  r@(c, o, e) <- liftIO $ runTranscoder ctl args'
  focusIO $ logMsg t ("transcode " ++ unwords args' ++ ": " ++ case c of { ExitSuccess -> "" ; ExitFailure i -> ": exit " ++ show i ++ "\n" } ++ o ++ e)
  return r

transcodeArgs :: Transcode -> ActionM TranscodeArgs
transcodeArgs t@Transcode{..} = do
  Just f <- getAssetFile transcodeOrig
  req <- peek
  auth <- peeks $ transcodeAuth t
  return $
    [ "-s", toFilePath f
    , "-r", BSLC.unpack $ BSB.toLazyByteString $ routeURL (Just req) remoteTranscode (transcodeId t) <> BSB.string8 "?auth=" <> BSB.byteString auth
    , "--" ]
    ++ maybe [] (\l -> ["-ss", show l]) lb
    ++ maybe [] (\u -> ["-t", show $ u - fromMaybe 0 lb]) (upperBound rng)
    ++ transcodeOptions
  where
  rng = segmentRange transcodeSegment
  lb = lowerBound rng

startTranscode :: Transcode -> ActionM (Maybe TranscodePID)
startTranscode tc = do
  tc' <- updateTranscode tc lock Nothing
  unless (transcodeProcess tc' == lock) $ fail $ "startTranscode " ++ show (transcodeId tc)
  findMatchingTranscode tc >>= maybe
    (do
      args <- transcodeArgs tc
      (r, out, err) <- ctlTranscode tc' args
      let pid = guard (r == ExitSuccess) >> readMaybe out
      _ <- updateTranscode tc' pid $ (isNothing pid ?> out) <> (null err ?!> err)
      return pid)
    (\Transcode{ transcodeAsset = match } -> do
      a <- changeAsset (transcodeAsset tc)
        { assetSHA1 = assetSHA1 match
        , assetDuration = assetDuration match
        , assetSize = assetSize match
        } Nothing
      void $ changeAssetSlotDuration a
      _ <- updateTranscode tc' Nothing (Just $ "reuse " ++ show (assetId match))
      return Nothing)
  where lock = Just (-1)

forkTranscode :: Transcode -> ActionM ThreadId
forkTranscode tc = focusIO $ \ctx ->
  forkAction
    (startTranscode tc) ctx
    (either
      (\e -> logMsg (view ctx) ("forkTranscode: " ++ show e) (view ctx))
      (const $ return ()))

stopTranscode :: Transcode -> ActionM Transcode
stopTranscode tc@Transcode{ transcodeProcess = Just pid } | pid >= 0 = do
  tc' <- updateTranscode tc Nothing (Just "aborted")
  (r, out, err) <- ctlTranscode tc ["-k", show pid]
  unless (r == ExitSuccess) $
    fail ("stopTranscode: " ++ out ++ err)
  return tc'
stopTranscode tc = return tc

collectTranscode :: Transcode -> Int -> Maybe BS.ByteString -> String -> ActionM ()
collectTranscode tc 0 sha1 logs = do
  tc' <- updateTranscode tc (Just (-2)) (Just logs)
  f <- makeTempFile (const $ return ())
  (r, out, err) <- ctlTranscode tc ["-c", BSC.unpack $ tempFilePath f]
  _ <- updateTranscode tc' Nothing (Just $ out ++ err)
  if r /= ExitSuccess
    then fail $ "collectTranscode " ++ show (transcodeId tc) ++ ": " ++ show r ++ "\n" ++ out ++ err
    else do
      av <- focusIO $ avProbe (tempFilePath f)
      unless (avProbeCheckFormat (assetFormat (transcodeAsset tc)) av)
        $ fail $ "collectTranscode " ++ show (transcodeId tc) ++ ": format error"
      let dur = avProbeLength av
      a <- changeAsset (transcodeAsset tc)
        { assetSHA1 = sha1
        , assetDuration = dur
        } (Just $ tempFilePath f)
      void $ changeAssetSlotDuration a
collectTranscode tc e _ logs =
  void $ updateTranscode tc Nothing (Just $ "exit " ++ show e ++ '\n' : logs)
