{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, ConstraintKinds, DefaultSignatures, GeneralizedNewtypeDeriving, TypeFamilies, OverloadedStrings #-}
module Databrary.Service.DB
  ( DBPool 
  , DBConn
  , initDB
  , finiDB
  , withDB
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

  , runDBConnection
  , useTDB
  , runTDB
  ) where

import Control.Applicative ((<$>))
import Control.Exception (onException, tryJust, bracket)
import Control.Monad (unless, (<=<))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT(..))
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

import Databrary.Has

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

newtype DBPool = PGPool (Pool PGConnection)
type DBConn = PGConnection

initDB :: C.Config -> IO DBPool
initDB conf = do
  db <- getPGDatabase conf
  stripes <- C.lookupDefault 1 conf "stripes"
  idle <- C.lookupDefault 300 conf "idle"
  conn <- C.lookupDefault 16 conf "maxconn"
  PGPool <$> createPool
    (pgConnect db)
    pgDisconnect
    stripes (fromRational idle) conn

finiDB :: DBPool -> IO ()
finiDB (PGPool p) = do
  destroyAllResources p

withDB :: DBPool -> (DBConn -> IO a) -> IO a
withDB (PGPool p) = withResource p

type MonadDB c m = (MonadIO m, MonadHas DBConn c m)

{-# INLINE liftDB #-}
liftDB :: MonadDB c m => (PGConnection -> IO a) -> m a
liftDB = focusIO

type DBM a = ReaderT PGConnection IO a

liftDBM :: MonadDB c m => DBM a -> m a
liftDBM q = liftDB $ runReaderT q

-- |Combination of 'liftDBM' and lifted 'tryJust'
dbTryJust :: MonadDB c m => (PGError -> Maybe e) -> DBM a -> m (Either e a)
dbTryJust err q = liftDB $ tryJust err . runReaderT q

dbRunQuery :: (MonadDB c m, PGQuery q a) => q -> m (Int, [a])
dbRunQuery q = liftDB $ \c -> pgRunQuery c q

dbExecute :: (MonadDB c m, PGQuery q ()) => q -> m Int
dbExecute q = liftDB $ \c -> pgExecute c q

dbExecuteSimple :: MonadDB c m => PGSimpleQuery () -> m Int
dbExecuteSimple = dbExecute

dbExecute1 :: (MonadDB c m, PGQuery q ()) => q -> m Bool
dbExecute1 q = do
  r <- dbExecute q
  case r of
    0 -> return False
    1 -> return True
    _ -> fail $ "pgExecute1: " ++ show r ++ " rows"

dbExecute1' :: (MonadDB c m, PGQuery q ()) => q -> m ()
dbExecute1' q = do
  r <- dbExecute1 q
  unless r $ fail $ "pgExecute1': failed"

dbExecute_ :: (MonadDB c m) => BSL.ByteString -> m ()
dbExecute_ q = liftDB $ \c -> pgSimpleQueries_ c q

dbQuery :: (MonadDB c m, PGQuery q a) => q -> m [a]
dbQuery q = liftDB $ \c -> pgQuery c q

dbQuery1 :: (MonadDB c m, PGQuery q a) => q -> m (Maybe a)
dbQuery1 q = do
  r <- dbQuery q
  case r of
    [] -> return $ Nothing
    [x] -> return $ Just x
    _ -> fail "pgQuery1: too many results"

dbQuery1' :: (MonadDB c m, PGQuery q a) => q -> m a
dbQuery1' = maybe (fail "pgQuery1': no results") return <=< dbQuery1

dbTransaction :: MonadDB c m => DBM a -> m a
dbTransaction f = liftDB $ \c -> do
  _ <- pgSimpleQuery c "BEGIN"
  onException (do
    r <- runReaderT f c
    _ <- pgSimpleQuery c "COMMIT"
    return r)
    (pgSimpleQuery c "ROLLBACK")


-- For connections outside runtime:

loadPGDatabase :: IO PGDatabase
loadPGDatabase = getPGDatabase . C.subconfig "db" =<< C.load [C.Required "databrary.conf"]

runDBConnection :: DBM a -> IO a
runDBConnection f = bracket
  (pgConnect =<< loadPGDatabase)
  pgDisconnect
  (runReaderT f)

loadTDB :: TH.DecsQ
loadTDB = useTPGDatabase =<< TH.runIO loadPGDatabase

{-# NOINLINE usedTDB #-}
usedTDB :: IORef Bool
usedTDB = unsafePerformIO $ newIORef False
useTDB :: TH.DecsQ
useTDB = do
  d <- TH.runIO $ atomicModifyIORef' usedTDB ((,) True)
  if d
    then return []
    else loadTDB

runTDB :: DBM a -> TH.Q a
runTDB f = do
  _ <- useTDB
  TH.runIO $ withTPGConnection $ runReaderT f
