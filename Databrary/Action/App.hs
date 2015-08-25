{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Action.App 
  ( AppRequest(..)
  , MonadAppAction 
  , AppActionM
  , AppAction
  , runApp
  , withAuth
  , withoutAuth
  , withReAuth
  ) where

import Control.Monad.Reader (ReaderT(..), asks, runReaderT, withReaderT)
import Control.Monad.Trans.Resource (InternalState, runResourceT, withInternalState)
import qualified Data.ByteString.Char8 as BSC
import Data.Time (getCurrentTime)
import Network.HTTP.Types (hDate)
import qualified Network.Wai as Wai

import Databrary.Has (view, peeks, makeHasRec)
import Databrary.HTTP
import Databrary.Service.Types
import Databrary.Service.Log
import Databrary.Model.Time
import Databrary.Action.Types
import Databrary.Model.Identity
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.HTTP.Request
import Databrary.Action.Response
import Databrary.Controller.Analytics

data AppRequest = AppRequest
  { appService :: !Service
  , appResourceState :: !InternalState
  , appTimestamp :: !Timestamp
  , appRequest :: !Request
  , appIdentity :: !Identity
  }

makeHasRec ''AppRequest ['appService, 'appResourceState, 'appTimestamp, 'appRequest, 'appIdentity]

type AppActionM a = ActionM AppRequest a
type AppAction = Action AppRequest

type MonadAppAction q m = (MonadHasAppRequest q m, ActionData q)

runApp :: Service -> AppAction -> Wai.Application
runApp rc act req send = do
  ts <- getCurrentTime
  runResourceT $ withInternalState $ \is -> do
    r <- runResult (runReaderT act (AppRequest rc is ts req PreIdentified))
    logAccess ts req r (serviceLogs rc)
    send r

instance ActionData AppRequest where
  returnResponse s h r = do
    ts <- asks appTimestamp
    h' <- peeks $ foldIdentity h ((: h) . (,) "user" . BSC.pack . (show :: Id Party -> String) . view)
    return $ response s ((hDate, formatHTTPTimestamp ts) : h') r

withAuth :: AppAction -> AppAction
withAuth f = do
  i <- determineIdentity
  withReaderT (\a -> a{ appIdentity = i }) $
    angularAnalytics >> f

withoutAuth :: AppAction -> AppAction
withoutAuth f =
  withReaderT (\a -> a{ appIdentity = NotIdentified }) $
    angularAnalytics >> f

withReAuth :: SiteAuth -> AppAction -> AppAction
withReAuth u =
  withReaderT (\a -> a{ appIdentity = ReIdentified u })
