{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, TemplateHaskell, OverloadedStrings #-}
module Databrary.Action.Types
  ( Context(..)
  , MonadHasContext
  , ActionM
  , runActionM
  , Action
  , runAction
  , forkAction
  , withAuth
  , withoutAuth
  , withReAuth
  ) where

import Control.Applicative (Applicative, Alternative, (<$>))
import Control.Concurrent (ThreadId, forkFinally)
import Control.Exception (SomeException)
import Control.Monad (MonadPlus)
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT(..), withReaderT)
import Control.Monad.Trans.Control (MonadBaseControl(..))
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
  , contextTimestamp :: !Timestamp
  , contextRequest :: !Request
  , contextResourceState :: !InternalState
  , contextDB :: !DBConn
  , contextIdentity :: !Identity
  }

makeHasRec ''Context ['contextService, 'contextDB, 'contextResourceState, 'contextTimestamp, 'contextRequest, 'contextIdentity]

newtype ActionM a = ActionM { unActionM :: ReaderT Context IO a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO, MonadBase IO, MonadThrow, MonadReader Context)

{-# INLINE runActionM #-}
runActionM :: ActionM a -> Context -> IO a
runActionM (ActionM (ReaderT f)) = f

instance MonadResource ActionM where
  liftResourceT = focusIO . runInternalState

instance MonadBaseControl IO ActionM where
  type StM ActionM a = a
  liftBaseWith f = ActionM $ liftBaseWith $ \r -> f (r . unActionM)
  restoreM = ActionM . restoreM

data Action = Action
  { _actionAuth :: !Bool
  , _actionM :: !(ActionM Response)
  }

withActionState :: Service -> (InternalState -> DBConn -> IO a) -> IO a
withActionState rc f =
  runResourceT $ withInternalState $ \is ->
    withDB (serviceDB rc) $ f is

runAction :: Service -> Action -> Wai.Application
runAction rc (Action auth act) req send = do
  ts <- getCurrentTime
  (i, r) <- withActionState rc $ \is db -> do
    let cf = Context rc ts req is db
        cp = cf PreIdentified
    ctx <- if auth then cf <$> runActionM determineIdentity cp else return cp
    r <- runResult $ runActionM (angularAnalytics >> act) ctx
    return (contextIdentity ctx, r)
  logAccess ts req (foldIdentity Nothing (Just . (show :: Id Party -> String) . view) i) r (serviceLogs rc)
  send $ Wai.mapResponseHeaders ((hDate, formatHTTPTimestamp ts) :) r

forkAction :: ActionM a -> Context -> (Either SomeException a -> IO ()) -> IO ThreadId
forkAction f c = forkFinally
  (withActionState (contextService c) $ \is db ->
    runActionM f c{ contextResourceState = is, contextDB = db })

withAuth :: ActionM Response -> Action
withAuth = Action True

withoutAuth :: ActionM Response -> Action
withoutAuth = Action False

withReAuth :: SiteAuth -> ActionM a -> ActionM a
withReAuth u = ActionM . withReaderT (\a -> a{ contextIdentity = ReIdentified u }) . unActionM
