{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, TemplateHaskell, OverloadedStrings #-}
module Databrary.Action.Types
  ( RequestContext(..)
  , MonadHasRequestContext
  , ActionM
  , runActionM
  , Action
  , runAction
  , forkAction
  , withAuth
  , withoutAuth
  , withReAuth
  ) where

import Control.Applicative (Applicative, Alternative)
import Control.Concurrent (ThreadId, forkFinally)
import Control.Exception (SomeException)
import Control.Monad (MonadPlus)
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT(..), withReaderT)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Trans.Resource (MonadThrow, MonadResource(..), runInternalState)
import Data.Time (getCurrentTime)
import Network.HTTP.Types (hDate)
import qualified Network.Wai as Wai

import Databrary.Has
import Databrary.HTTP
import Databrary.Service.Types
import Databrary.Service.Log
import Databrary.Model.Identity
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.HTTP.Request
import Databrary.Context
import Databrary.Action.Response
import Databrary.Controller.Analytics

data RequestContext = RequestContext
  { requestContext :: !Context
  , contextRequest :: !Request
  , requestIdentity :: !Identity
  }

makeHasRec ''RequestContext ['requestContext, 'contextRequest, 'requestIdentity]

newtype ActionM a = ActionM { unActionM :: ReaderT RequestContext IO a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO, MonadBase IO, MonadThrow, MonadReader RequestContext)

{-# INLINE runActionM #-}
runActionM :: ActionM a -> RequestContext -> IO a
runActionM (ActionM (ReaderT f)) = f

instance MonadResource ActionM where
  liftResourceT = focusIO . runInternalState

instance MonadBaseControl IO ActionM where
  type StM ActionM a = a
  liftBaseWith f = ActionM $ liftBaseWith $ \r -> f (r . unActionM)
  restoreM = ActionM . restoreM

withActionM :: Request -> Identity -> ActionM a -> ContextM a
withActionM r i = withReaderT (\c -> RequestContext c r i) . unActionM

data Action = Action
  { _actionAuth :: !Bool
  , _actionM :: !(ActionM Response)
  }

runAction :: Service -> Action -> Wai.Application
runAction rc (Action auth act) req send = do
  ts <- getCurrentTime
  (i, r) <- runContextM (do
    i <- if auth then withActionM req PreIdentified determineIdentity else return PreIdentified
    r <- ReaderT $ \ctx -> runResult $ runActionM (angularAnalytics >> act) (RequestContext ctx req i)
    return (i, r))
    rc
  logAccess ts req (foldIdentity Nothing (Just . (show :: Id Party -> String) . view) i) r (serviceLogs rc)
  send $ Wai.mapResponseHeaders ((hDate, formatHTTPTimestamp ts) :) r

forkAction :: ActionM a -> RequestContext -> (Either SomeException a -> IO ()) -> IO ThreadId
forkAction f (RequestContext c r i) = forkFinally $
  runContextM (withActionM r i f) (contextService c)

withAuth :: ActionM Response -> Action
withAuth = Action True

withoutAuth :: ActionM Response -> Action
withoutAuth = Action False

withReAuth :: SiteAuth -> ActionM a -> ActionM a
withReAuth u = ActionM . withReaderT (\a -> a{ requestIdentity = ReIdentified u }) . unActionM
