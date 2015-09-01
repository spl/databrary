module Databrary.Service.Periodic
  ( forkPeriodic
  ) where

import Control.Concurrent (ThreadId, forkFinally, threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.Fixed (Fixed(..), Micro)
import Data.Time.Clock (UTCTime(..), diffUTCTime, getCurrentTime)
import Data.Time.LocalTime (TimeOfDay(TimeOfDay), timeOfDayToTime)

import Databrary.Has (view, focusIO)
import Databrary.Service.Types
import Databrary.Service.Log
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

daily :: Service -> IO ()
daily = runReaderT $ do
  t <- liftIO getCurrentTime
  focusIO $ logMsg t "running daily cleanup"
  cleanTokens
  updateVolumeIndex
  ReaderT $ updateIndex t

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
