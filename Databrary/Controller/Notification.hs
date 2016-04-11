{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Notification
  ( Notice(..)
  , Notification(..)
  , blankNotification
  , createNotification
  , viewNotifications
  ) where

import Control.Monad (when)

import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.Notification
import Databrary.HTTP.Path.Parser
import Databrary.Controller.Permission
import Databrary.Action

-- |Must all have same target
sendNotifications :: MonadDB c m => [Notification] -> Delivery -> m ()
sendNotifications nl@(Notification{ notificationTarget = t }:_) d = do
  _ <- changeNotificationsDelivery nl d
  return ()
sendNotifications [] _ = return ()

createNotification :: Notification -> ActionM ()
createNotification n' = do
  d <- lookupNotify (notificationTarget n') (notificationNotice n')
  when (d > DeliveryNone) $ do
    n <- addNotification n'
    when (d >= DeliveryAsync) $ sendNotifications [n] d
    return ()

viewNotifications :: ActionRoute ()
viewNotifications = action GET (pathJSON </< "notifications") $ \() -> withAuth $ do
  _ <- authAccount
  nl <- lookupNotifications
  changeNotificationsDelivery nl DeliverySite
  return $ okResponse [] $ JSON.mapRecords (\n -> JSON.Record (notificationId n) $ mempty) nl
