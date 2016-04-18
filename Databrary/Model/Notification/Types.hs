{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
module Databrary.Model.Notification.Types
  ( module Databrary.Model.Notification.Notice
  , Notification(..)
  ) where

import Databrary.Model.Time
import Databrary.Model.Id
import Databrary.Model.Kind
import Databrary.Model.Party.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Segment
import Databrary.Model.Asset.Types
import Databrary.Model.Comment.Types
import Databrary.Model.Tag.Types
import Databrary.Model.Permission
import Databrary.Model.Notification.Notice

type instance IdType Notification = Int32

data Notification = Notification
  { notificationId :: Id Notification
  , notificationTarget :: !Account
  , notificationNotice :: !Notice
  , notificationTime :: Timestamp
  , notificationDelivered :: !Delivery
  , notificationAgent :: !PartyRow
  , notificationPartyId :: Maybe (Id Party)
  , notificationPermission :: Maybe Permission
  , notificationVolumeId :: Maybe (Id Volume)
  , notificationContainerId :: Maybe (Id Container)
  , notificationSegment :: Maybe Segment
  , notificationAssetId :: Maybe (Id Asset)
  , notificationCommentId :: Maybe (Id Comment)
  , notificationTagId :: Maybe (Id Tag)
  }

instance Kinded Notification where
  kindOf _ = "notification"
