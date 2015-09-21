{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TupleSections #-}
module Databrary.Service.Periodic
  ( forkPeriodic
  , Period(..)
  ) where

import Control.Concurrent (ThreadId, forkFinally, threadDelay)
import Control.Exception (Exception, handle)
import Control.Monad (void, when)
import Control.Monad.Trans.Reader (withReaderT)
import Data.Fixed (Fixed(..), Micro)
import Data.Time.Calendar.OrdinalDate (sundayStartWeek)
import Data.Time.Clock (UTCTime(..), diffUTCTime, getCurrentTime)
import Data.Time.LocalTime (TimeOfDay(TimeOfDay), timeOfDayToTime)
import Data.Typeable (Typeable)

import Databrary.Has
import Databrary.Service.Types
import Databrary.Service.Log
import Databrary.Context
import Databrary.Model.Token
import Databrary.Model.Volume
import Databrary.Solr.Index
import Databrary.EZID.Volume -- TODO

data Period
  = PeriodDaily
  | PeriodWeekly
  deriving (Typeable, Eq, Ord, Enum, Show)

instance Exception Period

threadDelay' :: Micro -> IO ()
threadDelay' (MkFixed t)
  | t > m' = threadDelay m >> threadDelay' (MkFixed (t - m'))
  | otherwise = threadDelay (fromInteger t)
  where
  m' = toInteger m
  m = maxBound

run :: Period -> Service -> IO ()
run p = runContextM $ withReaderT BackgroundContext $ do
  t <- peek
  focusIO $ logMsg t "running daily cleanup"
  cleanTokens
  updateVolumeIndex
  updateIndex
  when (p >= PeriodWeekly) $
    void updateEZID

runPeriodic :: Service -> IO ()
runPeriodic rc = loop (if s <= st then d s else s) where
  st = serviceStartTime rc
  s = st{ utctDayTime = timeOfDayToTime $ TimeOfDay 7 0 0 }
  d t = t{ utctDay = succ (utctDay t) }
  loop t = do
    n <- getCurrentTime
    (t', p) <- handle (return . (t ,)) $ do
      threadDelay' $ realToFrac $ diffUTCTime t n
      return (d t, if 0 == snd (sundayStartWeek (utctDay t))
        then PeriodWeekly
        else PeriodDaily)
    run p rc
    loop t'

forkPeriodic :: Service -> IO ThreadId
forkPeriodic rc = forkFinally (runPeriodic rc) $ \r -> do
  t <- getCurrentTime
  logMsg t ("periodic aborted: " ++ show r) (view rc)
