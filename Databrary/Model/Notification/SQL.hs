{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Notification.SQL
  ( selectNotification
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Language.Haskell.TH as TH

import Databrary.Has
import Databrary.Model.SQL.Select
import Databrary.Model.Time
import Databrary.Model.Id.Types
import Databrary.Model.Identity.Types
import Databrary.Model.Permission
import Databrary.Model.Party.Types
import Databrary.Model.Party.SQL
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Container.SQL
import Databrary.Model.Segment
import Databrary.Model.Asset.Types
import Databrary.Model.Tag.Types
import Databrary.Model.Comment.Types
import Databrary.Model.Slot.Types
import Databrary.Model.Notification.Types

makeNotification :: Id Notification -> Notice -> Timestamp -> Maybe Delivery -> Maybe (Id Party) -> Maybe Permission -> Maybe (Id Volume) -> Maybe (Id Container) -> Maybe Segment -> Maybe (Id Asset) -> Maybe (Id Comment) -> Maybe (Id Tag) -> Party -> Identity -> Notification
makeNotification i n t d p r v c s a m g w u = Notification i (view u) n t d w p r v c s a m g

notificationRow :: Selector -- ^ @'Party' -> 'Account' -> 'Notification'@
notificationRow = selectColumns 'makeNotification "notification" ["id", "notice", "time", "delivered", "party", "permission", "volume", "container", "segment", "asset", "comment", "tag"]

selectAgentTargetNotification :: Selector -- ^ @'Party' -> 'Account' -> 'Notification'@
selectAgentTargetNotification = notificationRow

selectNotification :: TH.Name -- ^ 'Identity'
  -> Selector -- ^ @'Notification'@
selectNotification ident = selectMap (`TH.AppE` TH.VarE ident) $ selectJoin '($)
  [ selectAgentTargetNotification
  , joinOn "notification.agent = party.id"
    $ selectParty ident
  ]
