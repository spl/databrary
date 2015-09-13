{-# LANGUAGE TemplateHaskell #-}
module Databrary.Service.Periodic
  ( forkPeriodic
  ) where

import Control.Concurrent (ThreadId, forkFinally, threadDelay)
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.Fixed (Fixed(..), Micro)
import Data.Time.Clock (UTCTime(..), diffUTCTime, getCurrentTime)
import Data.Time.LocalTime (TimeOfDay(TimeOfDay), timeOfDayToTime)

import Databrary.Has
import Databrary.Service.Types
import Databrary.Service.DB
import Databrary.Service.Log
import Databrary.Model.Time
import Databrary.Model.Token
import Databrary.Model.Volume
import Databrary.Solr.Index -- TODO

threadDelay' :: Micro -> IO ()
threadDelay' (MkFixed t)
  | t > m' = threadDelay m >> threadDelay' (MkFixed (t - m'))
  | otherwise = threadDelay (fromInteger t)
  where
  m' = toInteger m
  m = maxBound

data PeriodicContext = PeriodicContext
  { periodicService :: !Service
  , periodicTimestamp :: !Timestamp
  , periodicDB :: !DBConn
  }

makeHasRec ''PeriodicContext ['periodicService, 'periodicTimestamp, 'periodicDB]

type PeriodicM a = ReaderT PeriodicContext IO a

runPeriodicM :: PeriodicM a -> Service -> IO a
runPeriodicM f rc = do
  t <- getCurrentTime
  withDB (serviceDB rc) $ runReaderT f . PeriodicContext rc t

daily :: Service -> IO ()
daily = runPeriodicM $ do
  t <- peek
  focusIO $ logMsg t "running daily cleanup"
  cleanTokens
  updateVolumeIndex
  updateIndex

runPeriodic :: Service -> IO ()
runPeriodic rc = loop (if s <= st then d s else s) where
  st = serviceStartTime rc
  s = st{ utctDayTime = timeOfDayToTime $ TimeOfDay 7 0 0 }
  d t = t{ utctDay = succ (utctDay t) }
  loop t = do
    n <- getCurrentTime
    threadDelay' $ realToFrac $ diffUTCTime t n
    daily rc
    loop $ d t

forkPeriodic :: Service -> IO ThreadId
forkPeriodic rc = forkFinally (runPeriodic rc) $ \r -> do
  t <- getCurrentTime
  logMsg t ("periodic aborted: " ++ show r) (view rc)
