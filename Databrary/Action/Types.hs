{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Action.Types
  ( Context(..)
  , MonadAction 
  , ActionM
  , Action
  , runAction
  , withAuth
  , withoutAuth
  , withReAuth
  ) where

import Control.Monad.Reader (ReaderT(..), runReaderT, withReaderT)
import Control.Monad.Trans.Resource (InternalState, runResourceT, withInternalState)
import Data.Time (getCurrentTime)
import Network.HTTP.Types (hDate)
import qualified Network.Wai as Wai

import Databrary.Has (view, makeHasRec)
import Databrary.HTTP
import Databrary.Service.Types
import Databrary.Service.Log
import Databrary.Model.Time
import Databrary.Model.Identity
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.HTTP.Request
import Databrary.Action.Response
import Databrary.Controller.Analytics

data Context = Context
  { contextService :: !Service
  , contextResourceState :: !InternalState
  , contextTimestamp :: !Timestamp
  , contextRequest :: !Request
  , contextIdentity :: !Identity
  }

makeHasRec ''Context ['contextService, 'contextResourceState, 'contextTimestamp, 'contextRequest, 'contextIdentity]

type ActionM a = ReaderT Context IO a
type MonadAction q m = MonadHasContext q m

data Action = Action
  { _actionAuth :: !Bool
  , _actionM :: !(ActionM Response)
  }

runAction :: Service -> Action -> Wai.Application
runAction rc (Action auth act) req send = do
  ts <- getCurrentTime
  runResourceT $ withInternalState $ \is -> do
    let ctx = Context rc is ts req PreIdentified
    i <- if auth then runReaderT determineIdentity ctx else return PreIdentified
    r <- runResult $ runReaderT (angularAnalytics >> act) ctx{ contextIdentity = i }
    logAccess ts req (foldIdentity Nothing (Just . (show :: Id Party -> String) . view) i) r (serviceLogs rc)
    send $ Wai.mapResponseHeaders ((hDate, formatHTTPTimestamp ts) :) r

withAuth :: ActionM Response -> Action
withAuth = Action True

withoutAuth :: ActionM Response -> Action
withoutAuth = Action False

withReAuth :: SiteAuth -> ActionM a -> ActionM a
withReAuth u = withReaderT (\a -> a{ contextIdentity = ReIdentified u })
