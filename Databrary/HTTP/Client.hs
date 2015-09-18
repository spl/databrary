{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Databrary.HTTP.Client
  ( HTTPClient
  , initHTTPClient
  , httpRequest
  , httpRequestJSON
  , CookiesT
  , httpRequestCookies
  ) where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Exception (handle)
import Control.Monad.Trans.State.Strict (StateT(..))
import qualified Data.Aeson as JSON
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Monoid ((<>))
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types (hAccept, hContentType, ok200)

type HTTPClient = HC.Manager

initHTTPClient :: IO HTTPClient
initHTTPClient = HC.newManager HC.defaultManagerSettings
  { HC.managerConnCount = 2
  , HC.managerIdleConnectionCount = 4
  }

contentType :: BS.ByteString -> BS.ByteString
contentType = BSC.takeWhile (';' /=)

responseContentType :: HC.Response a -> Maybe BS.ByteString
responseContentType = fmap contentType . lookup hContentType . HC.responseHeaders

httpRequest :: HC.Request -> BS.ByteString -> (HC.Response HC.BodyReader -> IO (Maybe a)) -> HTTPClient -> IO (Maybe a)
httpRequest req acc f hcm = do
  handle (\(_ :: HC.HttpException) -> return Nothing) $
    HC.withResponse req { HC.requestHeaders = (hAccept, acc) : HC.requestHeaders req } hcm $ \res ->
      if HC.responseStatus res == ok200 && responseContentType res == Just (contentType acc)
        then f res
        else return Nothing

httpRequestJSON :: HC.Request -> HTTPClient -> IO (Maybe JSON.Value)
httpRequestJSON req = httpRequest req "application/json" $ \r ->
  P.maybeResult <$> P.parseWith (HC.responseBody r) JSON.json BS.empty

type CookiesT m a = StateT HC.CookieJar m a

httpRequestCookies :: HC.Request -> BS.ByteString -> (HC.Response HC.BodyReader -> IO (Maybe a)) -> HTTPClient -> CookiesT IO (Maybe a)
httpRequestCookies req acc f hcm = StateT $ \c -> maybe (Nothing, c) (first Just) <$>
  httpRequest req{ HC.cookieJar = HC.cookieJar req <> Just c } acc (\r -> fmap (, HC.responseCookieJar r) <$> f r) hcm
