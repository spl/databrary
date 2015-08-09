{-# LANGUAGE OverloadedStrings #-}
module Databrary.Search.Service
  ( SolrClient(..)
  , initSolr
  , MonadSolr
  ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Network.HTTP.Client as HC

import Databrary.Has (MonadHas)
import Databrary.HTTP.Client (HTTPClient)

newtype SolrClient = SolrClient
  { solrRequest :: HC.Request
  }

initSolr :: C.Config -> IO SolrClient
initSolr conf = do
  port <- C.require conf "port"
  req <- HC.parseUrl "http://localhost/solr/Databrary/"
  return SolrClient
    { solrRequest = req
      { HC.port = port
      , HC.redirectCount = 0
      }
    }

type MonadSolr c m = (MonadIO m, MonadHas HTTPClient c m, MonadHas SolrClient c m)
