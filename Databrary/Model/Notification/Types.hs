{-# LANGUAGE TypeFamilies #-}
module Databrary.Model.Notification.Types
  ( module Databrary.Model.Notification.Notice
  , Notification(..)
  ) where

import qualified Data.Aeson as JSON

import Databrary.Model.Time
import Databrary.Model.Id
import Databrary.Model.Party.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Notification.Notice

type instance IdType Notification = Int32

data Notification = Notification
  { notificationId :: Id Notification
  , notificationTarget :: Account
  , notificationNotice :: Notice
  , notificationTime :: Timestamp
  , notificationAgent :: Party
  , notificationVolumeId :: Maybe (Id Volume)
  , notificationData :: JSON.Value
  , notificationDelivered :: Maybe Delivery
  }
