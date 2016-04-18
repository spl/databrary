{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Notification
  ( Notice(..)
  , Notification(..)
  , blankNotification
  , createNotification
  , viewNotifications
  , deleteNotification
  ) where

import Control.Monad (when, forM_)
import Data.Function (on)
import Data.List (groupBy)
import Network.HTTP.Types (StdMethod(DELETE), noContent204)

import Databrary.Has
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.Model.Notification
import Databrary.HTTP.Path.Parser
import Databrary.Controller.Permission
import Databrary.Controller.Paths
import Databrary.Action

sendNotifications :: MonadDB c m => Delivery -> m ()
sendNotifications d = do
  unl <- lookupUndeliveredNotifications d
  forM_ (groupBy ((==) `on` partyId . partyRow . accountParty . notificationTarget) unl) $ \nl@(Notification{ notificationTarget = u }:_) -> do
    return ()
  _ <- changeNotificationsDelivery unl d
  return ()
  
createNotification :: Notification -> ActionM ()
createNotification n' = do
  d <- lookupNotify (notificationTarget n') (notificationNotice n')
  when (d > DeliveryNone) $ do
    n <- addNotification n'
    -- when (d >= DeliveryAsync) $ sendNotifications [n] d
    return ()

viewNotifications :: ActionRoute ()
viewNotifications = action GET (pathJSON </< "notification") $ \() -> withAuth $ do
  _ <- authAccount
  nl <- lookupUserNotifications
  _ <- changeNotificationsDelivery (filter ((DeliverySite >) . notificationDelivered) nl) DeliverySite -- would be nice if it could be done as part of lookupNotifications
  return $ okResponse [] $ JSON.mapRecords (\n -> JSON.Record (notificationId n) mempty) nl

deleteNotification :: ActionRoute (Id Notification)
deleteNotification = action DELETE (pathJSON >/> pathId) $ \i -> withAuth $ do
  _ <- authAccount
  r <- removeNotification i
  if r
    then return $ emptyResponse noContent204 []
    else peeks notFoundResponse
