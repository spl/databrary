{-# LANGUAGE OverloadedStrings #-}
module Databrary.Service.Log
  ( Logs
  , MonadLog
  , initLogs
  , finiLogs
  , LogStr
  , toLogStr
  , requestLog
  , logMsg
  , logAccess
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Data.Foldable as Fold
import Data.Maybe (fromMaybe, catMaybes)
import Data.Monoid ((<>))
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Time.Format (formatTime)
import Data.Time.LocalTime (ZonedTime, utcToLocalZonedTime)
import qualified Data.Traversable as Trav
import qualified Network.HTTP.Types as HTTP
import qualified Network.Socket as Net
import qualified Network.Wai as Wai
import System.Locale (defaultTimeLocale)
import System.Log.FastLogger

import Databrary.Has (MonadHas)
import Databrary.Model.Time

data Logs = Logs
  { loggerMessages, loggerAccess :: Maybe LoggerSet
  }

type MonadLog c m = (MonadHas Logs c m, MonadIO m)

initLog :: C.Config -> IO (Maybe LoggerSet)
initLog conf = do
  file <- C.lookup conf "file"
  buf <- C.lookupDefault defaultBufSize conf "buf"
  Trav.mapM (open buf) file
  where
  open buf "stdout" = newStdoutLoggerSet buf
  open buf "stderr" = newStderrLoggerSet buf
  open buf file = do
    num <- C.lookup conf "rotate"
    size <- C.lookupDefault (1024*1024) conf "size"
    let spec = FileLogSpec file size
    check $ spec $ fromMaybe 0 num
    Fold.mapM_ (rotate . spec) num
    newFileLoggerSet buf file

initLogs :: C.Config -> IO Logs
initLogs conf = Logs
  <$> initLog (C.subconfig "messages" conf)
  <*> initLog (C.subconfig "access" conf)

finiLogs :: Logs -> IO ()
finiLogs (Logs lm la) =
  Fold.mapM_ flushLogStr $ catMaybes [lm, la]

str :: ToLogStr a => a -> LogStr
str = toLogStr

char :: Char -> LogStr
char = str . BSC.singleton

pad :: ToLogStr a => Int -> a -> LogStr
pad n m
  | n < 0 = s <> p
  | otherwise = p <> s
  where
  s = str m
  p = str $ BSC.replicate (abs n - logStrLength s) ' '

quote :: Show a => Maybe a -> LogStr
quote = maybe (char '-') (str . show) -- FIXME, inefficient

time :: ZonedTime -> LogStr
time = str . formatTime defaultTimeLocale "%F %X"

infixr 6 &
(&) :: LogStr -> LogStr -> LogStr
x & y = x <> char ' ' <> y

logStr :: LoggerSet -> Timestamp -> LogStr -> IO ()
logStr l t s = do
  zt <- utcToLocalZonedTime t
  pushLogStr l $ time zt & s <> char '\n'

requestLog :: Timestamp -> Wai.Request -> Maybe String -> Wai.Response -> IO LogStr
requestLog qt q u r = do
  (Just h, Nothing) <- Net.getNameInfo [Net.NI_NUMERICHOST] True False $ Wai.remoteHost q
  rt <- getCurrentTime
  return
    $ pad (-15) h
    & pad 3 (show $ HTTP.statusCode $ Wai.responseStatus r)
    & pad 4 (fromMaybe "-" u)
    & pad 4 (show (floor $ 1000 * rt `diffUTCTime` qt :: Integer))
    & str (Wai.requestMethod q)
    & str (Wai.rawPathInfo q) <> str (Wai.rawQueryString q)
    & quote (lookup "location" rh)
    & quote (lookup "referer" qh)
    & quote (lookup "user-agent" qh)
  where
  qh = Wai.requestHeaders q
  rh = Wai.responseHeaders r

logMsg :: ToLogStr a => Timestamp -> a -> Logs -> IO ()
logMsg t m Logs{ loggerMessages = Just l } = do
  logStr l t $ str m
logMsg _ _ _ = return ()

logAccess :: Timestamp -> Wai.Request -> Maybe String -> Wai.Response -> Logs -> IO ()
logAccess qt q u r Logs{ loggerAccess = Just l } =
  logStr l qt =<< requestLog qt q u r
logAccess _ _ _ _ _ = return ()
