{-# LANGUAGE TemplateHaskell, QuasiQuotes, DataKinds, RecordWildCards #-}
module Databrary.Model.Notification
  ( module Databrary.Model.Notification.Types
  , module Databrary.Model.Notification.Notify
  , blankNotification
  , addNotification
  , deliveredNotifications
  ) where

import Database.PostgreSQL.Typed (pgSQL)

import Databrary.Has
import Databrary.Service.DB
import Databrary.Model.Party.Types
import Databrary.Model.Notification.Types
import Databrary.Model.Notification.Notify

useTDB

blankNotification :: Account -> Notice -> Notification
blankNotification target notice = Notification
  { notificationId = error "blankNotification"
  , notificationTarget = target
  , notificationNotice = notice
  , notificationTime = error "blankNotification"
  , notificationDelivered = Nothing
  , notificationAgent = error "blankNotification"
  , notificationPartyId = Nothing
  , notificationPermission = Nothing
  , notificationVolumeId = Nothing
  , notificationContainerId = Nothing
  , notificationSegment = Nothing
  , notificationAssetId = Nothing
  , notificationCommentId = Nothing
  , notificationTagId = Nothing
  }

addNotification :: (MonadDB c m, MonadHas Party c m) => Notification -> m Notification
addNotification n@Notification{..} = do
  p <- peek
  (i, t) <- dbQuery1' [pgSQL|INSERT INTO notification (target, notice, delivered, agent, party, volume, container, segment, asset, comment, tag, permission) VALUES (${partyId $ partyRow $ accountParty notificationTarget}, ${notificationNotice}, ${notificationDelivered}, ${partyId $ partyRow p}, ${notificationPartyId}, ${notificationVolumeId}, ${notificationContainerId}, ${notificationSegment}, ${notificationAssetId}, ${notificationCommentId}, ${notificationTagId}, ${notificationPermission}) RETURNING id, time|]
  return n
    { notificationId = i
    , notificationTime = t
    , notificationAgent = p
    }

deliveredNotifications :: MonadDB c m => [Notification] -> Delivery -> m Int
deliveredNotifications nl d =
  dbExecute [pgSQL|UPDATE notification SET delivered = ${d} WHERE id = ANY (${map notificationId nl})|]
