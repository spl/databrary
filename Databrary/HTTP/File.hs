{-# LANGUAGE OverloadedStrings #-}
module Databrary.HTTP.File
  ( fileResponse
  , serveFile
  ) where

import Control.Monad (when, mfilter)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS
import qualified Data.Foldable as Fold
import Data.Monoid ((<>))
import Network.HTTP.Types (ResponseHeaders, hLastModified, hContentType, hCacheControl, hIfModifiedSince, notModified304, hIfRange)
import qualified Network.Wai as Wai

import Databrary.Ops
import Databrary.Has (peek)
import Databrary.Files
import Databrary.HTTP.Request
import Databrary.HTTP
import Databrary.Action
import Databrary.Model.Format

fileResponse :: (MonadAction c m, MonadIO m) => RawFilePath -> Format -> Maybe BS.ByteString -> BS.ByteString -> m (ResponseHeaders, Maybe Wai.FilePart)
fileResponse file fmt save etag = do
  (sz, mt) <- maybeAction =<< liftIO (fileInfo file)
  let szi = toInteger sz
      fh = 
        [ ("etag", quoteHTTP etag)
        , (hLastModified, formatHTTPTimestamp mt)
        , (hContentType, formatMimeType fmt)
        , ("content-disposition", maybe "inline" (\n -> "attachment; filename="
            <> quoteHTTP (addFormatExtension n fmt)) save)
        , (hCacheControl, "max-age=31556926, private")
        ]
  req <- peek
  let ifnm = map unquoteHTTP $ (splitHTTP =<<) $ lookupRequestHeaders "if-none-match" req
      notmod
        | null ifnm = Fold.any (mt <=) $ (parseHTTPTimestamp =<<) $ lookupRequestHeader hIfModifiedSince req
        | otherwise = any (\m -> m == "*" || m == etag) ifnm
  when notmod $ result $ emptyResponse notModified304 fh
  let ifrng = unquoteHTTP <$> lookupRequestHeader hIfRange req
      part = mfilter (etag /=) ifrng $> -- allow range detection
        Wai.FilePart 0 szi szi -- force full file
  return (fh, part)

serveFile :: (MonadAction c m, MonadIO m) => RawFilePath -> Format -> Maybe BS.ByteString -> BS.ByteString -> m Response
serveFile file fmt save etag = do
  (fh, part) <- fileResponse file fmt save etag
  return $ okResponse fh (file, part)
