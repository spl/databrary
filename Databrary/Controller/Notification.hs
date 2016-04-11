module Databrary.Controller.Notification
  ( Notice(..)
  , Notification(..)
  , blankNotification
  , createNotification
  ) where

import Control.Monad (when)

import Databrary.Service.DB
import Databrary.Model.Notification
import Databrary.Action

-- |Must all have same target
sendNotifications :: MonadDB c m => [Notification] -> Delivery -> m ()
sendNotifications nl@(Notification{ notificationTarget = t }:_) d = do
  _ <- deliveredNotifications nl d
  return ()
sendNotifications [] _ = return ()

createNotification :: Notification -> ActionM ()
createNotification n' = do
  d <- lookupNotify (notificationTarget n') (notificationNotice n')
  when (d > DeliveryNone) $ do
    n <- addNotification n'
    when (d >= DeliveryAsync) $ sendNotifications [n] d
    return ()
    
