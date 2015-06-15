{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}
module Databrary.Service.Init
  ( loadConfig
  , withService
  ) where

import Control.Exception (bracket)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Time.Clock (getCurrentTime)
import System.FilePath.Posix ((</>))

import Paths_databrary (getSysconfDir, getDataFileName)
import Databrary.Service.DB (initDB, finiDB)
import Databrary.Service.Entropy (initEntropy, finiEntropy)
import Databrary.HTTP.Client (initHTTPClient, finiHTTPClient)
import Databrary.Store.Service (initStorage)
import Databrary.Service.Passwd (initPasswd)
import Databrary.Service.Log (initLogs, finiLogs)
import Databrary.Service.Messages (initMessages)
import Databrary.Web.Service (initWeb)
import Databrary.Store.AV (initAV)
import Databrary.Service.Types

loadConfig :: IO C.Config
loadConfig = do
  etc <- getSysconfDir
  msg <- getDataFileName "messages.conf"
  C.loadGroups [("message.", C.Required msg), ("", C.Optional (etc </> "databrary.conf")), ("", C.Required "local.conf")]

initService :: C.Config -> IO Service
initService conf = do
  time <- getCurrentTime
  logs <- initLogs (C.subconfig "log" conf)
  secret <- C.require conf "secret"
  entropy <- initEntropy
  passwd <- initPasswd
  authaddr <- C.require conf "authorize"
  messages <- initMessages (C.subconfig "message" conf)
  db <- initDB (C.subconfig "db" conf)
  storage <- initStorage (C.subconfig "store" conf)
  web <- initWeb
  httpc <- initHTTPClient
  av <- initAV
  return $ Service
    { serviceStartTime = time
    , serviceSecret = Secret secret
    , serviceEntropy = entropy
    , servicePasswd = passwd
    , serviceAuthorizeAddr = authaddr
    , serviceLogs = logs
    , serviceMessages = messages
    , serviceDB = db
    , serviceStorage = storage
    , serviceWeb = web
    , serviceHTTPClient = httpc
    , serviceAV = av
    }

finiService :: Service -> IO ()
finiService Service{..} = do
  finiHTTPClient serviceHTTPClient
  finiDB serviceDB
  finiEntropy serviceEntropy
  finiLogs serviceLogs

withService :: C.Config -> (Service -> IO a) -> IO a
withService c = bracket (initService c) finiService
