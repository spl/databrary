{-# LANGUAGE OverloadedStrings #-}
module Databrary.HTTP.Request
  ( Wai.Request
  , MonadHasRequest
  , lookupRequestHeader
  , lookupRequestHeaders
  , lookupQueryParameters
  , boolParameterValue
  , boolQueryParameter
  , requestHost
  ) where

import qualified Data.ByteString as BS
import qualified Data.Foldable as Fold
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Network.HTTP.Types (HeaderName)
import qualified Network.Wai as Wai

import Databrary.Has (MonadHas)

type MonadHasRequest c m = MonadHas Wai.Request c m

lookupRequestHeader :: HeaderName -> Wai.Request -> Maybe BS.ByteString
lookupRequestHeader h = lookup h . Wai.requestHeaders

lookupRequestHeaders :: HeaderName -> Wai.Request -> [BS.ByteString]
lookupRequestHeaders h = map snd . filter ((h ==) . fst) . Wai.requestHeaders

lookupQueryParameters :: BS.ByteString -> Wai.Request -> [Maybe BS.ByteString]
lookupQueryParameters q = map snd . filter ((q ==) . fst) . Wai.queryString

boolValue :: BS.ByteString -> Bool
boolValue "0" = False
boolValue "false" = False
boolValue "off" = False
boolValue "" = False
boolValue _ = True

boolParameterValue :: Maybe BS.ByteString -> Bool
boolParameterValue = Fold.all boolValue

boolQueryParameter :: BS.ByteString -> Wai.Request -> Bool
boolQueryParameter q = any boolParameterValue . lookupQueryParameters q

requestHost :: Wai.Request -> BS.ByteString
requestHost req =
  (if Wai.isSecure req then "https://" else "http://")
  <> fromMaybe "databrary.org" (Wai.requestHeaderHost req)
