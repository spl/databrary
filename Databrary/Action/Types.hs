{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings #-}
module Databrary.Action.Types
  ( Context(..)
  , MonadHasContext
  , ActionM
  , runActionM
  , Action
  , runAction
  , withAuth
  , withoutAuth
  , withReAuth
  ) where

import Control.Applicative (Applicative, Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Base (MonadBase)
import Control.Monad.Reader (MonadReader, ReaderT(..), withReaderT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Resource (InternalState, MonadThrow, MonadResource(..), runInternalState, runResourceT, withInternalState)
import Data.Time (getCurrentTime)
import Network.HTTP.Types (hDate)
import qualified Network.Wai as Wai

import Databrary.Has
import Databrary.HTTP
import Databrary.Service.Types
import Databrary.Service.Log
import Databrary.Service.DB
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

newtype ActionM a = ActionM { unActionM :: ReaderT Context IO a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO, MonadBase IO, MonadThrow, MonadReader Context, MonadDB)

{-# INLINE runActionM #-}
runActionM :: ActionM a -> Context -> IO a
runActionM (ActionM (ReaderT f)) = f

instance MonadResource ActionM where
  liftResourceT = focusIO . runInternalState

data Action = Action
  { _actionAuth :: !Bool
  , _actionM :: !(ActionM Response)
  }

runAction :: Service -> Action -> Wai.Application
runAction rc (Action auth act) req send = do
  ts <- getCurrentTime
  runResourceT $ withInternalState $ \is -> do
    let ctx = Context rc is ts req PreIdentified
    i <- if auth then runActionM determineIdentity ctx else return PreIdentified
    r <- runResult $ runActionM (angularAnalytics >> act) ctx{ contextIdentity = i }
    logAccess ts req (foldIdentity Nothing (Just . (show :: Id Party -> String) . view) i) r (serviceLogs rc)
    send $ Wai.mapResponseHeaders ((hDate, formatHTTPTimestamp ts) :) r

withAuth :: ActionM Response -> Action
withAuth = Action True

withoutAuth :: ActionM Response -> Action
withoutAuth = Action False

withReAuth :: SiteAuth -> ActionM a -> ActionM a
withReAuth u = ActionM . withReaderT (\a -> a{ contextIdentity = ReIdentified u }) . unActionM
