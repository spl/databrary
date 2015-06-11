{-# LANGUAGE TemplateHaskell #-}
module Databrary.Action.App 
  ( AppRequest(..)
  , MonadAppAction 
  , AppActionM
  , AppAction
  , runApp
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT(..), asks, runReaderT)
import Control.Monad.Trans.Resource (InternalState, runResourceT, withInternalState)
import Data.Time (getCurrentTime)
import Network.HTTP.Types (hDate)
import qualified Network.Wai as Wai

import Databrary.Has (makeHasRec)
import Databrary.HTTP
import Databrary.Service.Types
import Databrary.Service.Log
import Databrary.Model.Time
import Databrary.Action.Types
import Databrary.HTTP.Request
import Databrary.Action.Response

data AppRequest = AppRequest
  { appService :: !Service
  , appResourceState :: !InternalState
  , appTimestamp :: !Timestamp
  , appRequest :: !Request
  }

makeHasRec ''AppRequest ['appService, 'appResourceState, 'appTimestamp, 'appRequest]

type AppActionM a = ActionM AppRequest a
type AppAction = Action AppRequest

type MonadAppAction q m = (MonadHasAppRequest q m, ActionData q)

runApp :: Service -> AppAction -> Wai.Application
runApp rc act req send = do
  ts <- liftIO getCurrentTime
  runResourceT $ withInternalState $ \is -> do
    r <- runResult (runReaderT act (AppRequest rc is ts req))
    logAccess ts req r (serviceLogs rc)
    send r

instance ActionData AppRequest where
  returnResponse s h r = do
    ts <- asks appTimestamp
    return $ response s ((hDate, formatHTTPTimestamp ts) : h) r
