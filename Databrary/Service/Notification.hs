{-# LANGUAGE OverloadedStrings #-}
module Databrary.Service.Notification
  ( Notifications(..)
  , initNotifications
  , triggerNotifications
  ) where

import Control.Concurrent.MVar (MVar, newMVar)
import qualified Data.ByteString as BS
import qualified Text.Regex.Posix as Regex

import Databrary.Model.Periodic (Period)
import qualified Databrary.Store.Config as C

data Notifications = Notifications
  { notificationsTrigger :: !(MVar (Maybe Period))
  , notificationsFilter :: !(BS.ByteString -> Bool)
  , notificationsCopy :: !(Maybe BS.ByteString)
  }

initNotifications :: C.Config -> IO Notifications
initNotifications conf = do
  t <- newMVar Nothing
  return Notifications
    { notificationsTrigger = t
    , notificationsFilter = Regex.matchTest (Regex.makeRegexOpts Regex.compIgnoreCase Regex.blankExecOpt (conf C.! "filter" :: BS.ByteString) :: Regex.Regex)
    , notificationsCopy = conf C.! "copy"
    }

triggerNotifications :: Maybe Period -> Notifications -> IO ()
triggerNotifications p Notifications{ notificationsTrigger = t } = return () -- TODO
  
  
