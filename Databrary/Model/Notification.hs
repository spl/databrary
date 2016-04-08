{-# LANGUAGE TemplateHaskell, QuasiQuotes, DataKinds, RecordWildCards #-}
module Databrary.Model.Notification
  ( module Databrary.Model.Notification.Types
  , blankNotification
  , addNotification
  ) where

import Database.PostgreSQL.Typed (pgSQL)

import Databrary.Service.DB
import Databrary.Model.Party.Types
import Databrary.Model.Notification.Types

useTDB

blankNotification :: Account -> Notice -> Party -> Notification
blankNotification target notice agent = Notification
  { notificationId = error "blankNotification"
  , notificationTarget = target
  , notificationNotice = notice
  , notificationTime = error "blankNotification"
  , notificationDelivered = Nothing
  , notificationAgent = agent
  , notificationPartyId = Nothing
  , notificationPermission = Nothing
  , notificationVolumeId = Nothing
  , notificationContainerId = Nothing
  , notificationSegment = Nothing
  , notificationAssetId = Nothing
  , notificationCommentId = Nothing
  , notificationTagId = Nothing
  }

addNotification :: MonadDB c m => Notification -> m Notification
addNotification n@Notification{..} = do
  (i, t) <- dbQuery1' [pgSQL|INSERT INTO notification (target, notice, delivered, agent, party, volume, container, segment, asset, comment, tag, permission) VALUES (${partyId $ partyRow $ accountParty notificationTarget}, ${notificationNotice}, ${notificationDelivered}, ${partyId $ partyRow notificationAgent}, ${notificationPartyId}, ${notificationVolumeId}, ${notificationContainerId}, ${notificationSegment}, ${notificationAssetId}, ${notificationCommentId}, ${notificationTagId}, ${notificationPermission}) RETURNING id, time|]
  return n
    { notificationId = i
    , notificationTime = t
    }
