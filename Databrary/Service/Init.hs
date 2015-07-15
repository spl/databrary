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
import Databrary.Service.DB (initDB, finiDB)
import Databrary.Service.Entropy (initEntropy, finiEntropy)
import Databrary.HTTP.Client (initHTTPClient, finiHTTPClient)
import Databrary.Store.Service (initStorage)
import Databrary.Store.AV (initAV)
import Databrary.Service.Passwd (initPasswd)
import Databrary.Service.Log (initLogs, finiLogs)
import Databrary.Service.Messages (initMessages)
import Databrary.Web.Service (initWeb)
import Databrary.Static.Service (initStatic)
import Databrary.Service.Types

loadConfig :: IO C.Config
loadConfig = do
  msg <- getDataFileName "messages.conf"
  C.loadGroups [("message.", C.Required msg), ("", C.Required "databrary.conf")]

initService :: C.Config -> IO Service
initService conf = do
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
  return $ Service
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
    }

finiService :: Service -> IO ()
finiService Service{..} = do
  finiHTTPClient serviceHTTPClient
  finiDB serviceDB
  finiEntropy serviceEntropy
  finiLogs serviceLogs

withService :: C.Config -> (Service -> IO a) -> IO a
withService c = bracket (initService c) finiService
