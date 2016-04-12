{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Notification
  ( Notice(..)
  , Notification(..)
  , blankNotification
  , createNotification
  , viewNotifications
  , deleteNotification
  ) where

import Control.Monad (when)
import Network.HTTP.Types (StdMethod(DELETE), noContent204)

import Databrary.Has
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.Id.Types
import Databrary.Model.Notification
import Databrary.HTTP.Path.Parser
import Databrary.Controller.Permission
import Databrary.Controller.Paths
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
viewNotifications = action GET (pathJSON </< "notification") $ \() -> withAuth $ do
  _ <- authAccount
  nl <- lookupNotifications
  _ <- changeNotificationsDelivery nl DeliverySite
  return $ okResponse [] $ JSON.mapRecords (\n -> JSON.Record (notificationId n) $ mempty) nl

deleteNotification :: ActionRoute (Id Notification)
deleteNotification = action DELETE (pathJSON >/> pathId) $ \i -> withAuth $ do
  _ <- authAccount
  r <- removeNotification i
  if r
    then return $ emptyResponse noContent204 []
    else peeks notFoundResponse
