{-# LANGUAGE OverloadedStrings #-}
module Databrary.EZID.Service
  ( EZID(..)
  , initEZID
  ) where

import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Data.Traversable as Trav
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types (hContentType)

data EZID = EZID
  { ezidRequest :: !HC.Request
  , ezidNS :: !BS.ByteString
  }

initEZID :: C.Config -> IO (Maybe EZID)
initEZID conf = C.lookup conf "ns" >>= Trav.mapM (\ns -> do
  unless ("doi:10." `BSC.isPrefixOf` ns) $
    fail "ezid.ns must be for DOIs"
  user <- C.require conf "user"
  pass <- C.require conf "pass"
  req <- HC.parseUrl "https://ezid.cdlib.org/"
  return $ EZID
    { ezidRequest = HC.applyBasicAuth user pass req
      { HC.requestHeaders = (hContentType, "text/plain") : HC.requestHeaders req
      , HC.responseTimeout = Just 100000000
      , HC.redirectCount = 1
      }
    , ezidNS = ns
    })
