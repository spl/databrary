{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}
module Databrary.Service.Init
  ( loadConfig
  , withService
  ) where

import Control.Exception (bracket)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Time.Clock (getCurrentTime)

import Paths_databrary (getDataFileName)
import Databrary.Ops
import Databrary.Service.DB (initDB, finiDB)
import Databrary.Service.Entropy (initEntropy)
import Databrary.HTTP.Client (initHTTPClient)
import Databrary.Store.Service (initStorage)
import Databrary.Store.AV (initAV)
import Databrary.Service.Passwd (initPasswd)
import Databrary.Service.Log (initLogs, finiLogs)
import Databrary.Service.Messages (initMessages)
import Databrary.Web.Service (initWeb)
import Databrary.Static.Service (initStatic)
import Databrary.Ingest.Service (initIngest)
import Databrary.Solr.Service (initSolr, finiSolr)
import Databrary.EZID.Service (initEZID)
import Databrary.Service.Periodic (forkPeriodic)
import Databrary.Service.Types

loadConfig :: IO C.Config
loadConfig = do
  msg <- getDataFileName "messages.conf"
  C.loadGroups [("message.", C.Required msg), ("", C.Required "databrary.conf")]

initService :: Bool -> C.Config -> IO Service
initService fg conf = do
  time <- getCurrentTime
  logs <- initLogs (C.subconfig "log" conf)
  secret <- C.require conf "secret"
  entropy <- initEntropy
  passwd <- initPasswd
  messages <- initMessages (C.subconfig "message" conf)
  db <- initDB (C.subconfig "db" conf)
  storage <- initStorage (C.subconfig "store" conf)
  av <- initAV
  web <- initWeb
  httpc <- initHTTPClient
  static <- initStatic (C.subconfig "static" conf)
  solr <- initSolr fg (C.subconfig "solr" conf)
  ezid <- initEZID (C.subconfig "ezid" conf)
  ingest <- initIngest
  let rc = Service
        { serviceStartTime = time
        , serviceSecret = Secret secret
        , serviceEntropy = entropy
        , servicePasswd = passwd
        , serviceLogs = logs
        , serviceMessages = messages
        , serviceDB = db
        , serviceStorage = storage
        , serviceAV = av
        , serviceWeb = web
        , serviceHTTPClient = httpc
        , serviceStatic = static
        , serviceIngest = ingest
        , serviceSolr = solr
        , serviceEZID = ezid
        , servicePeriodic = Nothing
        }
  periodic <- fg ?$> forkPeriodic rc
  return rc
    { servicePeriodic = periodic
    }

finiService :: Service -> IO ()
finiService Service{..} = do
  finiSolr serviceSolr
  finiDB serviceDB
  finiLogs serviceLogs

withService :: Bool -> C.Config -> (Service -> IO a) -> IO a
withService fg c = bracket (initService fg c) finiService
