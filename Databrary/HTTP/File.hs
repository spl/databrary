{-# LANGUAGE OverloadedStrings #-}
module Databrary.HTTP.File
  ( fileResponse
  , serveFile
  ) where

import Control.Monad (when, mfilter)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (toLower)
import qualified Data.Foldable as Fold
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Network.HTTP.Types (ResponseHeaders, hLastModified, hContentType, hCacheControl, hIfModifiedSince, notModified304, hIfRange, hContentEncoding)
import System.Posix.Types (FileOffset)

import Databrary.Ops
import Databrary.Has
import Databrary.Files
import Databrary.HTTP.Request
import Databrary.HTTP
import Databrary.Action
import Databrary.Model.Format

bsLCEq :: BS.ByteString -> BS.ByteString -> Bool
bsLCEq t s
  | BS.length t == BS.length s = t == BSC.map toLower s
  | otherwise = False

fileResponse :: RawFilePath -> Bool -> Format -> Maybe BS.ByteString -> BS.ByteString -> ActionM (ResponseHeaders, (RawFilePath, Maybe FileOffset))
fileResponse file agz fmt save etag = do
  (sz, mt) <- maybeAction =<< liftIO (fileInfo file)
  gzi <- liftIO $ if agz then mfilter ((sz >) . (24 +) . fst) <$> fileInfo filegz else return Nothing
  let gz = isJust gzi
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
  let part = maybe sz fst gzi <? -- allow range detection or force full file
        Fold.any ((etag /=) . unquoteHTTP) (lookupRequestHeader hIfRange req)
  return
    ( (if gz then ((hContentEncoding, "gzip") :) else id) fh
    , (if gz then filegz else file, part))
  where filegz = file <.> "gz"

serveFile :: RawFilePath -> Format -> Maybe BS.ByteString -> BS.ByteString -> ActionM Response
serveFile file fmt save etag = do
  agz <- any (bsLCEq "gzip") . concatMap splitHTTP <$> peeks (lookupRequestHeaders "accept-encoding")
  uncurry okResponse <$> fileResponse file agz fmt save etag
