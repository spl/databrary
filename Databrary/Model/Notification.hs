{-# LANGUAGE TemplateHaskell, QuasiQuotes, DataKinds, RecordWildCards, OverloadedStrings #-}
module Databrary.Model.Notification
  ( module Databrary.Model.Notification.Types
  , module Databrary.Model.Notification.Notify
  , addNotification
  , changeNotificationsDelivery
  , lookupUserNotifications
  , lookupUndeliveredNotifications
  , removeNotification
  , notificationJSON
  ) where

import Data.Monoid ((<>))
import Database.PostgreSQL.Typed (pgSQL)

import Databrary.Has
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Id.Types
import Databrary.Model.Party
import Databrary.Model.Volume.Types
import Databrary.Model.Tag.Types
import Databrary.Model.Notification.Types
import Databrary.Model.Notification.Notify
import Databrary.Model.Notification.SQL

useTDB

addNotification :: (MonadDB c m, MonadHas Party c m) => Notification -> m Notification
addNotification n@Notification{..} = do
  p <- peek
  (i, t) <- dbQuery1' [pgSQL|INSERT INTO notification (target, notice, delivered, agent, party, volume, container, segment, asset, comment, tag, permission) VALUES (${partyId $ partyRow $ accountParty notificationTarget}, ${notificationNotice}, ${notificationDelivered}, ${partyId $ partyRow p}, ${partyId <$> notificationParty}, ${volumeId <$> notificationVolume}, ${notificationContainerId}, ${notificationSegment}, ${notificationAssetId}, ${notificationCommentId}, ${tagId <$> notificationTag}, ${notificationPermission}) RETURNING id, time|]
  return n
    { notificationId = i
    , notificationTime = t
    , notificationAgent = partyRow p
    }

changeNotificationsDelivery :: MonadDB c m => [Notification] -> Delivery -> m Int
changeNotificationsDelivery nl d =
  dbExecute [pgSQL|UPDATE notification SET delivered = ${d} WHERE id = ANY (${map notificationId nl}) AND delivered < ${d}|]

lookupUserNotifications :: (MonadDB c m, MonadHas Account c m) => m [Notification]
lookupUserNotifications = do
  u <- peek
  dbQuery $ ($ u) <$> $(selectQuery selectTargetNotification "$WHERE target = ${view u :: Id Party} ORDER BY notification.id")

lookupUndeliveredNotifications :: MonadDB c m => Delivery -> m [Notification]
lookupUndeliveredNotifications d =
  dbQuery $(selectQuery selectNotification "JOIN notify_view USING (target, notice) WHERE delivery >= ${d} AND delivered = 'none' ORDER BY notification.target, notification.id")

removeNotification :: (MonadDB c m, MonadHas (Id Party) c m) => Id Notification -> m Bool
removeNotification i = do
  p <- peek
  dbExecute1 [pgSQL|DELETE FROM notification WHERE id = ${i} AND target = ${p :: Id Party}|]

notificationJSON :: JSON.ToNestedObject o u => Notification -> JSON.Record (Id Notification) o
notificationJSON Notification{..} = JSON.Record notificationId $
     "notice" JSON..= notificationNotice
  <> "time" JSON..= notificationTime
  <> "delivered" JSON..= notificationDelivered
  <> "agent" JSON..=. JSON.recordObject ({-on (==) partyId notificationAgent (partyRow (accountParty notificationTarget)) ?!>-} partyRowJSON notificationAgent)
  <> "party" JSON..=? (partyId <$> notificationParty)
  <> "permission" JSON..=? notificationPermission
  <> "volume" JSON..=? (volumeId <$> notificationVolume)
  <> "container" JSON..=? notificationContainerId
  <> "segment" JSON..=? notificationSegment
  <> "asset" JSON..=? notificationAssetId
  <> "comment" JSON..=? notificationCommentId
  <> "tag" JSON..=? (tagName <$> notificationTag)
