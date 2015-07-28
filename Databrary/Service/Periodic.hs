module Databrary.Service.Periodic
  ( forkPeriodic
  ) where

import Control.Concurrent (ThreadId, forkFinally, threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Fixed (Fixed(..), Micro)
import Data.Time.Clock (UTCTime(..), diffUTCTime, getCurrentTime)
import Data.Time.LocalTime (TimeOfDay(TimeOfDay), timeOfDayToTime)

import Databrary.Has (view, focusIO)
import Databrary.Service.Types
import Databrary.Service.Log
import Databrary.Model.Token
import Databrary.Model.Volume

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

runPeriodic :: Service -> IO ()
runPeriodic rc = do
  n <- getCurrentTime
  let s = n{ utctDayTime = timeOfDayToTime $ TimeOfDay 7 0 0 }
  loop (if s <= n then d s else s)
  where
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
