{-# LANGUAGE OverloadedStrings #-}
module Databrary.HTTP.Client
  ( HTTPClient
  , initHTTPClient
  , finiHTTPClient
  , HTTPClientM
  , httpRequest
  , httpRequestJSON
  ) where

import Control.Applicative ((<$>))
import Control.Exception (handle)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as JSON
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types (hAccept, hContentType, ok200)

import Databrary.Has (MonadHas, peek)

import Debug.Trace

type HTTPClient = HC.Manager

initHTTPClient :: IO HTTPClient
initHTTPClient = HC.newManager HC.defaultManagerSettings
  { HC.managerConnCount = 2
  , HC.managerIdleConnectionCount = 4
  }

finiHTTPClient :: HTTPClient -> IO ()
finiHTTPClient = HC.closeManager

type HTTPClientM c m = (MonadHas HTTPClient c m, MonadIO m)

contentType :: BS.ByteString -> BS.ByteString
contentType = BSC.takeWhile (';' /=)

responseContentType :: HC.Response a -> Maybe BS.ByteString
responseContentType = fmap contentType . lookup hContentType . HC.responseHeaders

httpRequest :: HTTPClientM c m => HC.Request -> BS.ByteString -> (HC.BodyReader -> IO (Maybe a)) -> m (Maybe a)
httpRequest req acc f = do
  hcm <- peek
  liftIO $ handle (\(e :: HC.HttpException) -> traceShowM e >> return Nothing) $
    HC.withResponse req { HC.requestHeaders = (hAccept, acc) : HC.requestHeaders req } hcm $ \res ->
      if traceShowId (HC.responseStatus res) == ok200 && traceShowId (responseContentType res) == Just (contentType acc)
        then f $ HC.responseBody res
        else return Nothing

httpRequestJSON :: HTTPClientM c m => HC.Request -> m (Maybe JSON.Value)
httpRequestJSON req = httpRequest req "application/json" $ \rb ->
  P.maybeResult <$> P.parseWith rb JSON.json BS.empty
