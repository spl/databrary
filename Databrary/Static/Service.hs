{-# LANGUAGE OverloadedStrings #-}
module Databrary.Static.Service
  ( Static(..)
  , initStatic
  ) where

import qualified Crypto.Hash as Hash
import qualified Data.ByteString as BS
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Data.Text as T

import Databrary.Model.URL

data Static = Static
  { staticAuthorizeAddr :: T.Text
  , staticInvestigator :: Maybe URI
  , staticKey :: BS.ByteString -> Hash.HMAC Hash.SHA256
  }

initStatic :: C.Config -> IO Static
initStatic conf = do
  authaddr <- C.require conf "authorize"
  fillin <- C.lookup conf "fillin"
  key <- C.lookupDefault "databrary" conf "key"
  return $ Static
    { staticAuthorizeAddr = authaddr
    , staticInvestigator = fillin
    , staticKey = Hash.hmac key
    }
