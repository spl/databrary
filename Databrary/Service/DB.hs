{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, ConstraintKinds, DefaultSignatures, GeneralizedNewtypeDeriving, TypeFamilies, OverloadedStrings #-}
module Databrary.Service.DB
  ( withPGConnection
  , DBConn
  , initDB
  , finiDB
  , MonadDB
  , DBM
  , liftDBM
  , dbTryJust
  , dbRunQuery
  , dbExecute
  , dbExecuteSimple
  , dbExecute1
  , dbExecute1'
  , dbExecute_
  , dbQuery
  , dbQuery1
  , dbQuery1'
  , dbTransaction
  , useTPG
  ) where

import Control.Applicative (Applicative, (<$>))
import Control.Exception (onException, tryJust, bracket)
import Control.Monad (unless, (<=<))
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.State (StateT)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.Maybe (fromMaybe, isJust)
import Data.Pool (Pool, withResource, createPool, destroyAllResources)
import Database.PostgreSQL.Typed.Protocol
import Database.PostgreSQL.Typed.Query
import Database.PostgreSQL.Typed.TH (withTPGConnection, useTPGDatabase)
import qualified Language.Haskell.TH as TH
import Network (PortID(..))
import System.IO.Unsafe (unsafePerformIO)

import Databrary.Has (MonadHas, Has, peek)

getPGDatabase :: C.Config -> IO PGDatabase
getPGDatabase conf = do
  host <- C.lookup conf "host"
  port <- C.lookupDefault (5432 :: Int) conf "port"
  sock <- C.lookupDefault "/tmp/.s.PGSQL.5432" conf "sock"
  user <- C.require conf "user"
  db <- C.lookupDefault user conf "db"
  passwd <- C.lookupDefault "" conf "pass"
  debug <- C.lookupDefault False conf "debug"
  return $ defaultPGDatabase
    { pgDBHost = fromMaybe "localhost" host
    , pgDBPort = if isJust host then PortNumber (fromIntegral port) else UnixSocket sock
    , pgDBName = db
    , pgDBUser = user
    , pgDBPass = passwd
    , pgDBDebug = debug
    }

newtype DBConn = PGPool (Pool PGConnection)

initDB :: C.Config -> IO DBConn
initDB conf = do
  db <- getPGDatabase conf
  stripes <- C.lookupDefault 1 conf "stripes"
  idle <- C.lookupDefault 300 conf "idle"
  conn <- C.lookupDefault 16 conf "maxconn"
  PGPool <$> createPool
    (pgConnect db)
    pgDisconnect
    stripes (fromRational idle) conn

finiDB :: DBConn -> IO ()
finiDB (PGPool p) = do
  destroyAllResources p

class (Functor m, Applicative m, Monad m) => MonadDB m where
  liftDB :: (PGConnection -> IO a) -> m a
  default liftDB :: (MonadIO m, MonadHas DBConn c m) => (PGConnection -> IO a) -> m a
  liftDB f = do
    PGPool db <- peek
    liftIO $ withResource db f

newtype DBM a = DBM { runDBM :: ReaderT PGConnection IO a } deriving (Functor, Applicative, Monad, MonadIO, MonadBase IO)

instance MonadBaseControl IO DBM where
  type StM DBM a = a
  liftBaseWith f = DBM $ liftBaseWith $ \r -> f (r . runDBM)
  restoreM = DBM . restoreM

instance MonadDB DBM where
  liftDB = DBM . ReaderT

instance (Functor m, Applicative m, MonadIO m, Has DBConn c) => MonadDB (ReaderT c m)
instance MonadDB m => MonadDB (StateT a m) where
  liftDB = lift . liftDB

liftDBM :: MonadDB m => DBM a -> m a
liftDBM (DBM q) = liftDB $ runReaderT q

-- |Combination of 'liftDBM' and lifted 'tryJust'
dbTryJust :: MonadDB m => (PGError -> Maybe e) -> DBM a -> m (Either e a)
dbTryJust err (DBM q) = liftDB $ tryJust err . runReaderT q

dbRunQuery :: (MonadDB m, PGQuery q a) => q -> m (Int, [a])
dbRunQuery q = liftDB $ \c -> pgRunQuery c q

dbExecute :: (MonadDB m, PGQuery q ()) => q -> m Int
dbExecute q = liftDB $ \c -> pgExecute c q

dbExecuteSimple :: MonadDB m => PGSimpleQuery () -> m Int
dbExecuteSimple = dbExecute

dbExecute1 :: (MonadDB m, PGQuery q ()) => q -> m Bool
dbExecute1 q = do
  r <- dbExecute q
  case r of
    0 -> return False
    1 -> return True
    _ -> fail $ "pgExecute1: " ++ show r ++ " rows"

dbExecute1' :: (MonadDB m, PGQuery q ()) => q -> m ()
dbExecute1' q = do
  r <- dbExecute1 q
  unless r $ fail $ "pgExecute1': failed"

dbExecute_ :: (MonadDB m) => BSL.ByteString -> m ()
dbExecute_ q = liftDB $ \c -> pgSimpleQueries_ c q

dbQuery :: (MonadDB m, PGQuery q a) => q -> m [a]
dbQuery q = liftDB $ \c -> pgQuery c q

dbQuery1 :: (MonadDB m, PGQuery q a) => q -> m (Maybe a)
dbQuery1 q = do
  r <- dbQuery q
  case r of
    [] -> return $ Nothing
    [x] -> return $ Just x
    _ -> fail "pgQuery1: too many results"

dbQuery1' :: (MonadDB m, PGQuery q a) => q -> m a
dbQuery1' = maybe (fail "pgQuery1': no results") return <=< dbQuery1

dbTransaction :: MonadDB m => DBM a -> m a
dbTransaction (DBM f) = liftDB $ \c -> do
  _ <- pgSimpleQuery c "BEGIN"
  onException (do
    r <- runReaderT f c
    _ <- pgSimpleQuery c "COMMIT"
    return r)
    (pgSimpleQuery c "ROLLBACK")


-- For connections outside runtime:

loadPGDatabase :: IO PGDatabase
loadPGDatabase = getPGDatabase . C.subconfig "db" =<< C.load [C.Required "databrary.conf"]

withPGConnection :: DBM a -> IO a
withPGConnection (DBM f) = bracket
  (pgConnect =<< loadPGDatabase)
  pgDisconnect
  (runReaderT f)

loadTPG :: TH.DecsQ
loadTPG = useTPGDatabase =<< TH.runIO loadPGDatabase

{-# NOINLINE usedTPG #-}
usedTPG :: IORef Bool
usedTPG = unsafePerformIO $ newIORef False
useTPG :: TH.DecsQ
useTPG = do
  d <- TH.runIO $ atomicModifyIORef' usedTPG ((,) True)
  if d
    then return []
    else loadTPG

instance MonadDB TH.Q where
  liftDB f = do
    _ <- useTPG
    TH.runIO $ withTPGConnection f
