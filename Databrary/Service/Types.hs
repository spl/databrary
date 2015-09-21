{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}
module Databrary.Service.Types
  ( Secret(..)
  , Service(..)
  , MonadHasService
  ) where

import Control.Concurrent (ThreadId)
import qualified Data.ByteString as BS

import Databrary.Has (makeHasRec)
import Databrary.Service.DB (DBPool)
import Databrary.Service.Entropy (Entropy)
import Databrary.HTTP.Client (HTTPClient)
import Databrary.Store.Types (Storage)
import Databrary.Store.AV (AV)
import Databrary.Service.Passwd (Passwd)
import Databrary.Service.Log (Logs)
import Databrary.Service.Messages (Messages)
import Databrary.Web.Types (Web)
import Databrary.Static.Service (Static)
import Databrary.Solr.Service (Solr)
import Databrary.Ingest.Service (Ingest)
import Databrary.EZID.Service (EZID)
import Databrary.Model.Time

newtype Secret = Secret BS.ByteString

data Service = Service
  { serviceStartTime :: !Timestamp
  , serviceSecret :: !Secret
  , serviceEntropy :: !Entropy
  , servicePasswd :: !Passwd
  , serviceLogs :: !Logs
  , serviceMessages :: !Messages
  , serviceDB :: !DBPool
  , serviceStorage :: !Storage
  , serviceAV :: !AV
  , serviceWeb :: !Web
  , serviceHTTPClient :: !HTTPClient
  , serviceStatic :: !Static
  , serviceIngest :: !Ingest
  , serviceSolr :: !Solr
  , serviceEZID :: !(Maybe EZID)
  , servicePeriodic :: !(Maybe ThreadId)
  }

makeHasRec ''Service ['serviceSecret, 'serviceEntropy, 'servicePasswd, 'serviceLogs, 'serviceMessages, 'serviceDB, 'serviceStorage, 'serviceAV, 'serviceWeb, 'serviceHTTPClient, 'serviceStatic, 'serviceIngest, 'serviceSolr]
