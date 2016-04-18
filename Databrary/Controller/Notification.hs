{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Notification
  ( Notice(..)
  , Notification(..)
  , blankNotification
  , createNotification
  , viewNotifications
  , deleteNotification
  , forkNotifier
  ) where

import Control.Concurrent (ThreadId, forkFinally, threadDelay)
import Control.Concurrent.MVar (takeMVar, tryTakeMVar)
import Control.Monad (when, forM_, join)
import Data.Function (on)
import Data.List (groupBy)
import Data.Time.Clock (getCurrentTime)
import Network.HTTP.Types (StdMethod(DELETE), noContent204)

import Databrary.Has
import Databrary.Ops
import qualified Databrary.JSON as JSON
import Databrary.Service.Types
import Databrary.Service.Notification
import Databrary.Service.Log
import Databrary.Service.DB
import Databrary.Context
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.Model.Notification
import Databrary.HTTP.Path.Parser
import Databrary.Controller.Permission
import Databrary.Controller.Paths
import Databrary.Action

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

sendNotifications :: MonadDB c m => Delivery -> m ()
sendNotifications d = do
  unl <- lookupUndeliveredNotifications d
  forM_ (groupBy ((==) `on` partyId . partyRow . accountParty . notificationTarget) unl) $ \nl@(Notification{ notificationTarget = u }:_) -> do
    return ()
  _ <- changeNotificationsDelivery unl d
  return ()
  
runNotifier :: Service -> IO ()
runNotifier rc = loop where
  t = notificationsTrigger $ serviceNotification rc
  loop = do
    d' <- takeMVar t
    d <- d' `orElseM` do
      threadDelay 10000000 -- 10 second throttle on async notifications
      join <$> tryTakeMVar t
    runContextM (sendNotifications $ periodicDelivery d) rc
    loop

forkNotifier :: Service -> IO ThreadId
forkNotifier rc = forkFinally (runNotifier rc) $ \r -> do
  t <- getCurrentTime
  logMsg t ("notifier aborted: " ++ show r) (view rc)

