{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Notification
  ( viewNotify
  , postNotify
  , createNotification
  , viewNotifications
  , deleteNotification
  , forkNotifier
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent (ThreadId, forkFinally, threadDelay)
import Control.Concurrent.MVar (takeMVar, tryTakeMVar)
import Control.Monad (join, when, void)
import Data.Function (on)
import Data.List (groupBy)
import Data.Time.Clock (getCurrentTime)
import Network.HTTP.Types (StdMethod(DELETE), noContent204)
import qualified Text.Regex.Posix as Regex

import Databrary.Has
import Databrary.Ops
import qualified Databrary.JSON as JSON
import Databrary.Service.Types
import Databrary.Service.Notification
import Databrary.Service.Log
import Databrary.Service.Mail
import Databrary.Service.Messages
import Databrary.Context
import Databrary.Model.Id.Types
import Databrary.Model.Party
import Databrary.Model.Notification
import Databrary.HTTP.Path.Parser
import Databrary.HTTP.Form.Deform
import Databrary.Controller.Permission
import Databrary.Controller.Paths
import Databrary.Controller.Form
import Databrary.Action
import Databrary.View.Notification

viewNotify :: ActionRoute ()
viewNotify = action GET (pathJSON </< "notify") $ \() -> withAuth $ do
  u <- authAccount
  n <- lookupAccountNotify u
  return $ okResponse [] $ JSON.toEncoding n

postNotify :: ActionRoute ()
postNotify = action POST (pathJSON </< "notify") $ \() -> withAuth $ do
  u <- authAccount
  (nl, md) <- runForm Nothing $ do
    csrfForm
    (,)
      <$> ("notice" .:> return <$> deform <|> withSubDeforms (const deform))
      <*> ("delivery" .:> deformNonEmpty deform)
  mapM_ (maybe (void . removeNotify u) (\d n -> changeNotify u n d) md) nl
  return $ emptyResponse noContent204 []

createNotification :: Notification -> ActionM ()
createNotification n' = do
  d <- lookupNotify (notificationTarget n') (notificationNotice n')
  when (d > DeliveryNone) $ do
    n <- addNotification n'
      { notificationDelivered = if d == DeliveryImmediate then d else notificationDelivered n' }
    when (d >= DeliveryAsync) $
      if notificationDelivered n == DeliveryImmediate
        then sendTargetNotifications [n]
        else focusIO $ triggerNotifications Nothing

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

-- |Assumed to be all same target
sendTargetNotifications :: (MonadMail c m, MonadHas Notifications c m, MonadHas Messages c m) => [Notification] -> m ()
sendTargetNotifications l@(Notification{ notificationTarget = u }:_) = do
  Notifications{ notificationsFilter = filt, notificationsCopy = copy } <- peek
  msg <- peek
  sendMail (map Right (filter (Regex.matchTest filt . accountEmail) [u])) (maybe [] (return . Left) copy)
    "Databrary notifications"
    $ mailNotifications msg l
sendTargetNotifications [] = return ()

emitNotifications :: Delivery -> ContextM ()
emitNotifications d = do
  unl <- lookupUndeliveredNotifications d
  mapM_ sendTargetNotifications $ groupBy ((==) `on` partyId . partyRow . accountParty . notificationTarget) unl
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

