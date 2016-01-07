{-# LANGUAGE TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Databrary.Model.Comment.Types
  ( Comment(..)
  , CommentRow(..)
  ) where

import qualified Data.Text as T

import Databrary.Has (makeHasRec)
import Databrary.Model.Kind
import Databrary.Model.Time
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.Model.Slot.Types

type instance IdType Comment = Int32

data Comment = Comment
  { commentId :: Id Comment
  , commentWho :: Account
  , commentSlot :: Slot
  , commentTime :: Timestamp
  , commentText :: T.Text
  , commentParents :: [Id Comment]
  }

instance Kinded Comment where
  kindOf _ = "comment"

makeHasRec ''Comment ['commentId, 'commentWho, 'commentSlot, 'commentTime]

data CommentRow = CommentRow
  { commentRowId :: Id Comment
  , commentRowWhoId :: Id Party
  , commentRowSlotId :: SlotId
  , commentRowTime :: Timestamp
  , commentRowText :: T.Text
  }
