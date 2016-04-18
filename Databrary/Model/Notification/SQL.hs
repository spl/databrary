{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Notification.SQL
  ( selectUserNotification
  , selectNotification
  ) where

import qualified Language.Haskell.TH as TH

import Databrary.Has
import Databrary.Model.SQL.Select
import Databrary.Model.Time
import Databrary.Model.Id.Types
import Databrary.Model.Permission
import Databrary.Model.Party.Types
import Databrary.Model.Party.SQL
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Segment
import Databrary.Model.Asset.Types
import Databrary.Model.Tag.Types
import Databrary.Model.Comment.Types
import Databrary.Model.Notification.Types

makeNotification :: Id Notification -> Notice -> Timestamp -> Delivery -> Maybe (Id Party) -> Maybe Permission -> Maybe (Id Volume) -> Maybe (Id Container) -> Maybe Segment -> Maybe (Id Asset) -> Maybe (Id Comment) -> Maybe (Id Tag) -> Account -> PartyRow -> Notification
makeNotification i n t d p r v c s a m g u w = Notification i (view u) n t d w p r v c s a m g

notificationRow :: Selector -- ^ @'Account' -> 'Party' -> 'Notification'@
notificationRow = selectColumns 'makeNotification "notification" ["id", "notice", "time", "delivered", "party", "permission", "volume", "container", "segment", "asset", "comment", "tag"]

selectTargetAgentNotification :: Selector -- ^ @'Account' -> 'Party' -> 'Notification'@
selectTargetAgentNotification = notificationRow

selectUserNotification :: TH.Name -- ^ 'Account'
  -> Selector -- ^ @'Notification'@
selectUserNotification ident = selectJoin '($)
  [ selectMap (`TH.AppE` TH.VarE ident) selectTargetAgentNotification
  , joinOn "notification.agent = agent.id"
    $ selectPartyRow `fromAlias` "agent"
  ]

selectNotification :: Selector -- ^ @'Notification'@
selectNotification = selectJoin '($)
  [ selectTargetAgentNotification
  , joinOn "notification.target = account.id"
    selectUserAccount
  , joinOn "notification.agent = agent"
    $ selectPartyRow `fromAlias` "agent"
  ]
