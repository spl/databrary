{-# LANGUAGE TupleSections #-}
module Databrary.Ingest.Service
  ( IngestStatus(..)
  , Ingest
  , initIngest
  , getIngestStatus
  , runIngest
  , abortIngest
  , clearIngest
  ) where

import Control.Applicative ((<$>))
import Control.Arrow (left)
import Control.Concurrent (ThreadId, forkFinally, killThread)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, swapMVar, withMVar, modifyMVar, modifyMVar_)
import Control.Monad (join, void)
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.Int (Int32)
import qualified Data.Text as T

import Databrary.Has (Has, view)

data IngestStatus
  = IngestInactive
  | IngestActive ThreadId
  | IngestCompleted [Int32]
  | IngestFailed [T.Text]

newtype Ingest = Ingest
  { ingestStatus :: MVar IngestStatus
  }

initIngest :: IO Ingest
initIngest = Ingest <$> newMVar IngestInactive

getIngestStatus :: Ingest -> IO IngestStatus
getIngestStatus = readMVar . ingestStatus

runIngest :: Has Ingest c => ReaderT c IO (Either [T.Text] [Int32]) -> ReaderT c IO Bool
runIngest r = ReaderT $ \c -> let v = ingestStatus (view c) in
  modifyMVar v $ \s ->
    case s of
      IngestActive _ -> return (s, False)
      _ -> (, True) . IngestActive <$> forkFinally (runReaderT r c)
        (void . swapMVar v . either IngestFailed IngestCompleted . join . left (return . T.pack . show))

abortIngest :: Ingest -> IO ()
abortIngest Ingest{ ingestStatus = v } = withMVar v abt where
  abt (IngestActive t) = killThread t
  abt _ = return ()

clearIngest :: Ingest -> IO ()
clearIngest Ingest{ ingestStatus = v } = modifyMVar_ v clr where
  clr s@(IngestActive _) = return s
  clr _ = return IngestInactive
