{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Notification.SQL
  ( selectTargetNotification
  , selectNotification
  ) where

import Databrary.Has
import Databrary.Model.SQL.Select
import Databrary.Model.Time
import Databrary.Model.Id.Types
import Databrary.Model.Permission
import Databrary.Model.Party.Types
import Databrary.Model.Party.SQL
import Databrary.Model.Volume.Types
import Databrary.Model.Volume.SQL
import Databrary.Model.Container.Types
import Databrary.Model.Segment
import Databrary.Model.Asset.Types
import Databrary.Model.Tag.Types
import Databrary.Model.Tag.SQL
import Databrary.Model.Comment.Types
import Databrary.Model.Notification.Types

makeNotification :: Id Notification -> Notice -> Timestamp -> Delivery -> Maybe Permission -> Maybe (Id Container) -> Maybe Segment -> Maybe (Id Asset) -> Maybe (Id Comment) -> PartyRow -> Maybe PartyRow -> Maybe VolumeRow -> Maybe Tag -> Account -> Notification
makeNotification i n t d r c s a m w p v g u = Notification i (view u) n t d w p r v c s a m g

notificationRow :: Selector -- ^ @'PartyRow' -> Maybe 'PartyRow' -> Maybe 'VolumeRow' -> Maybe 'Tag' -> 'Account' -> 'Notification'@
notificationRow = selectColumns 'makeNotification "notification" ["id", "notice", "time", "delivered", "permission", "container", "segment", "asset", "comment"]

selectTargetNotification :: Selector -- ^ @'Account' -> 'Notification'@
selectTargetNotification = selectJoin '($)
  [ notificationRow
  , joinOn "notification.agent = agent.id"
    $ selectPartyRow `fromAlias` "agent"
  , maybeJoinOn "notification.party = nparty.id"
    $ selectPartyRow `fromAlias` "nparty"
  , maybeJoinOn "notification.volume = volume.id"
    $ selectVolumeRow
  , maybeJoinOn "notification.tag = tag.id"
    $ selectTag
  ]

selectNotification :: Selector -- ^ @'Notification'@
selectNotification = selectJoin '($)
  [ selectTargetNotification
  , joinOn "notification.target = account.id"
    selectUserAccount
  ]
