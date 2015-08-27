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

import Databrary.Model.URL

data Static = Static
  { staticAuthorizeAddr :: BS.ByteString
  , staticInvestigator :: Maybe URI
  , staticKey :: BS.ByteString -> HMAC Hash.SHA256
  }

initStatic :: C.Config -> IO Static
initStatic conf = do
  authaddr <- C.require conf "authorize"
  fillin <- C.lookup conf "fillin"
  key <- C.lookupDefault ("databrary" :: BS.ByteString) conf "key"
  return $ Static
    { staticAuthorizeAddr = authaddr
    , staticInvestigator = fillin
    , staticKey = hmac key
    }
