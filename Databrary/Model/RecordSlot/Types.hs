{-# LANGUAGE TypeFamilies #-}
module Databrary.Model.RecordSlot.Types
  ( RecordSlot(..)
  , recordSlotId
  ) where

import Control.Applicative ((<|>))

import Databrary.Has (Has(..))
import Databrary.Model.Id.Types
import Databrary.Model.Permission
import Databrary.Model.Release
import Databrary.Model.Segment
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Record.Types
import Databrary.Model.RecordCategory.Types
import Databrary.Model.Slot.Types

data RecordSlotId = RecordSlotId
  { _slotRecordId :: !(Id Record)
  , _recordSlotId :: !(Id Slot)
  }

type instance IdType RecordSlot = RecordSlotId

data RecordSlot = RecordSlot
  { slotRecord :: Record
  , recordSlot :: Slot
  }

recordSlotId :: RecordSlot -> Id RecordSlot
recordSlotId (RecordSlot r s) = Id $ RecordSlotId (recordId r) (slotId s)

instance Has Record RecordSlot where
  view = slotRecord
instance Has (Id Record) RecordSlot where
  view = view . slotRecord
instance Has RecordCategory RecordSlot where
  view = view . slotRecord
instance Has (Id RecordCategory) RecordSlot where
  view = view . slotRecord
instance Has Volume RecordSlot where
  view = view . slotRecord
instance Has (Id Volume) RecordSlot where
  view = view . slotRecord
instance Has Permission RecordSlot where
  view = view . slotRecord

instance Has Slot RecordSlot where
  view = recordSlot
instance Has Container RecordSlot where
  view = view . recordSlot
instance Has (Id Container) RecordSlot where
  view = view . recordSlot
instance Has Segment RecordSlot where
  view = view . recordSlot

instance Has (Maybe Release) RecordSlot where
  view rs = view (recordSlot rs) <|> view (slotRecord rs)
instance Has Release RecordSlot where
  view = view . (view :: RecordSlot -> Maybe Release)

