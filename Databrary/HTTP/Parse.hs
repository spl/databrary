{-# LANGUAGE OverloadedStrings #-}
module Databrary.HTTP.Parse
  ( Content(..)
  , FileContent
  , parseRequestContent
  ) where

import Control.Applicative ((<$>))
import Control.Monad (when, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (runReaderT)
import qualified Data.Aeson as JSON
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Monoid (mempty)
import Data.Word (Word64)
import Network.HTTP.Types (requestEntityTooLarge413, unsupportedMediaType415, hContentType)
import Network.Wai
import Network.Wai.Parse
import System.IO (Handle)

import Databrary.Has (peek, peeks)
import Databrary.Action.Types
import Databrary.Store.Temp
import Databrary.HTTP.Request (lookupRequestHeader)
import Databrary.Action.Response (response, result)

requestTooLarge :: Response
requestTooLarge = response requestEntityTooLarge413 [] (mempty :: BSB.Builder)

type ChunkParser a = IO BS.ByteString -> IO a

_mapChunks :: (a -> b) -> ChunkParser a -> ChunkParser b
_mapChunks f parse next = f <$> parse next

_nullChunks :: ChunkParser Word64
_nullChunks next = go 0 where
  go n = do
    b <- next
    if BS.null b
      then return n
      else go (n + fromIntegral (BS.length b))

limitChunks :: Word64 -> ChunkParser a -> ChunkParser a
limitChunks lim parse next = do
  len <- liftIO $ newIORef 0
  parse $ do
    n <- readIORef len
    b <- next
    let n' = n + fromIntegral (BS.length b)
    when (n' > lim) $ result requestTooLarge
    writeIORef len n'
    return b

writeChunks :: Handle -> ChunkParser ()
writeChunks h next = run where
  run = do
    b <- next
    unless (BS.null b) $
      BS.hPut h b >> run

parserChunks :: AP.Parser a -> ChunkParser (AP.Result a)
parserChunks parser next = run (AP.parse parser) where
  run p = do
    b <- next
    let r = p b
    if BS.null b
      then return r
      else run $ AP.feed r


_mapBackEnd :: (a -> b) -> BackEnd a -> BackEnd b
_mapBackEnd f back param info next = f <$> back param info next

rejectBackEnd :: BackEnd a
rejectBackEnd _ _ _ = result requestTooLarge


_parseRequestChunks :: (MonadIO m, MonadAction c m) => ChunkParser a -> m a
_parseRequestChunks p = liftIO . p =<< peeks requestBody

limitRequestChunks :: (MonadIO m, MonadAction c m) => Word64 -> ChunkParser a -> m a
limitRequestChunks lim p = do
  rq <- peek
  case requestBodyLength rq of
    KnownLength l | l > lim -> result requestTooLarge
    _ -> liftIO $ limitChunks lim p $ requestBody rq

data Content a
  = ContentForm 
    { contentFormParams :: [Param]
    , contentFormFiles :: [File a]
    }
  | ContentJSON JSON.Value
  | ContentUnknown

maxTextSize :: Word64
maxTextSize = 1024*1024

class FileContent a where
  parseFileContent :: IO BS.ByteString -> ActionM a

instance FileContent () where
  parseFileContent _ = result requestTooLarge

instance FileContent TempFile where
  parseFileContent = makeTempFile . flip writeChunks

instance FileContent JSON.Value where
  parseFileContent b = liftIO $ either (result . response unsupportedMediaType415 []) return . AP.eitherResult =<< parserChunks JSON.json b

parseFormContent :: (MonadIO m, MonadAction c m) => RequestBodyType -> m (Content a)
parseFormContent t = uncurry ContentForm
  <$> limitRequestChunks maxTextSize (liftIO . sinkRequestBody rejectBackEnd t)

parseFormFileContent :: (MonadIO m, MonadAction c m, FileContent a) => (FileInfo BS.ByteString -> Word64) -> RequestBodyType -> m (Content a)
parseFormFileContent ff rt = do
  app <- peek
  (p, f) <- liftIO $ do
    let be fn fi fb = case ff fi{ fileContent = fn } of
          0 -> result requestTooLarge
          m -> limitChunks m (\b -> runReaderT (parseFileContent b) app) fb
    sinkRequestBody be rt (requestBody $ contextRequest app)
  return $ ContentForm p f

parseJSONContent :: (MonadIO m, MonadAction c m) => m (Content a)
parseJSONContent = maybe ContentUnknown ContentJSON . AP.maybeResult
  <$> limitRequestChunks maxTextSize (liftIO . parserChunks JSON.json)

parseRequestContent :: (MonadIO m, MonadAction c m, FileContent a) => (BS.ByteString -> Word64) -> m (Content a)
parseRequestContent ff = do
  ct <- peeks $ lookupRequestHeader hContentType
  case fmap parseContentType ct of
    Just ("application/x-www-form-urlencoded", _) ->
      parseFormContent UrlEncoded
    Just ("multipart/form-data", attrs) | Just bound <- lookup "boundary" attrs ->
      parseFormFileContent (ff . fileContent) $ Multipart bound
    Just ("text/json", _) ->
      parseJSONContent
    Just ("application/json", _) ->
      parseJSONContent
    _ -> return ContentUnknown
