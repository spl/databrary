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
import Control.Monad (join, when, unless)
import Data.Function (on)
import Data.List (groupBy)
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as TL
import Data.Time.Clock (getCurrentTime)
import Network.HTTP.Types (StdMethod(DELETE), noContent204)
import qualified Text.Regex.Posix as Regex

import Databrary.Has
import Databrary.Ops
import qualified Databrary.JSON as JSON
import Databrary.Service.Types
import Databrary.Service.Notification
import Databrary.Service.Log
import Databrary.Service.DB
import Databrary.Service.Mail
import Databrary.Context
import Databrary.Model.Id.Types
import Databrary.Model.Party
import Databrary.Model.Notification
import Databrary.HTTP.Path.Parser
import Databrary.Controller.Permission
import Databrary.Controller.Paths
import Databrary.Action
import Databrary.View.Notification

createNotification :: Notification -> ActionM ()
createNotification n' = do
  d <- lookupNotify (notificationTarget n') (notificationNotice n')
  when (d > DeliveryNone) $ do
    n <- addNotification n'
      { notificationDelivered = if d == DeliveryImmediate then d else notificationDelivered n' }
    when (d >= DeliveryAsync) $ focusIO $
      if notificationDelivered n == DeliveryImmediate
        then mailNotifications [n]
        else triggerNotifications Nothing

viewNotifications :: ActionRoute ()
viewNotifications = action GET (pathJSON </< "notification") $ \() -> withAuth $ do
  _ <- authAccount
  nl <- lookupUserNotifications
  _ <- changeNotificationsDelivery (filter ((DeliverySite >) . notificationDelivered) nl) DeliverySite -- would be nice if it could be done as part of lookupNotifications
  return $ okResponse [] $ JSON.mapRecords (\n -> notificationJSON n JSON..<> "html" JSON..= htmlNotification n) nl

deleteNotification :: ActionRoute (Id Notification)
deleteNotification = action DELETE (pathJSON >/> pathId) $ \i -> withAuth $ do
  _ <- authAccount
  r <- removeNotification i
  if r
    then return $ emptyResponse noContent204 []
    else peeks notFoundResponse

mailNotifications :: [Notification] -> Notifications -> IO ()
mailNotifications l@(Notification{ notificationTarget = u }:_) s = unless (null to) $ do
  sendMail (map Right (filter (Regex.matchTest (notificationsFilter s) . accountEmail) [u])) (maybe [] (return . Left) $ notificationsCopy s)
    "Databrary notifications"
    $ TL.fromChunks ["Dear ", partyName $ partyRow $ accountParty u, ",\n"]
    <> foldMap (\n -> '\n' `TL.cons` mailNotification n `TL.snoc` '\n') l
  where
  to = map Right (filter (Regex.matchTest (notificationsFilter s) . accountEmail) [u]) ++ maybe [] (return . Left) (notificationsCopy s)
mailNotifications [] _ = return ()

emitNotifications :: (MonadDB c m, MonadHas Notifications c m) => Delivery -> m ()
emitNotifications d = do
  unl <- lookupUndeliveredNotifications d
  mapM_ (focusIO . mailNotifications) $ groupBy ((==) `on` partyId . partyRow . accountParty . notificationTarget) unl
  _ <- changeNotificationsDelivery unl d
  return ()
  
runNotifier :: Service -> IO ()
runNotifier rc = loop where
  t = notificationsTrigger $ serviceNotification rc
  loop = do
    d' <- takeMVar t
    d <- d' `orElseM` do
      threadDelay 60000000 -- 60 second throttle on async notifications
      join <$> tryTakeMVar t
    runContextM (emitNotifications $ periodicDelivery d) rc
    loop

forkNotifier :: Service -> IO ThreadId
forkNotifier rc = forkFinally (runNotifier rc) $ \r -> do
  t <- getCurrentTime
  logMsg t ("notifier aborted: " ++ show r) (view rc)

