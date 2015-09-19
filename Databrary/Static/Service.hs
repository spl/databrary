{-# LANGUAGE OverloadedStrings #-}
module Databrary.Static.Service
  ( Static(..)
  , initStatic
  ) where

import qualified Crypto.Hash.Algorithms as Hash
import Crypto.MAC.HMAC (HMAC, hmac)
import qualified Data.ByteString as BS
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Data.Traversable as Trav
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types (methodPost, hContentType)

data Static = Static
  { staticAuthorizeAddr :: BS.ByteString
  , staticInvestigator :: Maybe HC.Request
  , staticKey :: BS.ByteString -> HMAC Hash.SHA256
  }

initStatic :: C.Config -> IO Static
initStatic conf = do
  authaddr <- C.require conf "authorize"
  fillin <- Trav.mapM HC.parseUrl =<< C.lookup conf "fillin"
  key <- C.lookupDefault ("databrary" :: BS.ByteString) conf "key"
  return $ Static
    { staticAuthorizeAddr = authaddr
    , staticInvestigator = fmap (\f -> f
      { HC.method = methodPost
      , HC.requestHeaders = (hContentType, "application/x-www-form-urlencoded") : HC.requestHeaders f
      , HC.cookieJar = Nothing
      , HC.redirectCount = 0
      }) fillin
    , staticKey = hmac key
    }
