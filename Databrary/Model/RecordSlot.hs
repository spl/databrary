{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards, DataKinds, ViewPatterns #-}
module Databrary.Model.RecordSlot
  ( module Databrary.Model.RecordSlot.Types
  , lookupRecordSlots
  , lookupSlotRecords
  , lookupContainerRecords
  , lookupRecordSlotRecords
  , lookupVolumeContainersRecords
  , lookupVolumeContainersRecordIds
  , moveRecordSlot
  , recordSlotJSON
  ) where

import Control.Arrow (second)
import Control.Monad (guard, liftM2)
import Data.Function (on)
import Data.Maybe (catMaybes)
import qualified Database.PostgreSQL.Typed.Range as Range
import Database.PostgreSQL.Typed.Types (PGTypeName(..))

import Databrary.Ops
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.Id.Types
import Databrary.Model.Segment
import Databrary.Model.Permission
import Databrary.Model.Audit
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Slot
import Databrary.Model.Metric
import Databrary.Model.Record
import Databrary.Model.Age
import Databrary.Model.Measure
import Databrary.Model.SQL
import Databrary.Model.RecordSlot.Types
import Databrary.Model.RecordSlot.SQL

lookupRecordSlots :: (MonadDB m) => Record -> m [RecordSlot]
lookupRecordSlots r =
  dbQuery $ ($ r) <$> $(selectQuery selectRecordSlotRecord "$WHERE slot_record.record = ${recordId r}")

lookupSlotRecords :: (MonadDB m) => Slot -> m [RecordSlot]
lookupSlotRecords (Slot c s) =
  dbQuery $ ($ c) <$> $(selectQuery selectContainerSlotRecord "$WHERE slot_record.container = ${containerId c} AND slot_record.segment && ${s}")

lookupContainerRecords :: (MonadDB m) => Container -> m [RecordSlot]
lookupContainerRecords = lookupSlotRecords . containerSlot

lookupRecordSlotRecords :: (MonadDB m) => Record -> Slot -> m [RecordSlot]
lookupRecordSlotRecords r (Slot c s) =
  dbQuery $ ($ c) . ($ r) <$> $(selectQuery selectRecordContainerSlotRecord "WHERE slot_record.record = ${recordId r} AND slot_record.container = ${containerId c} AND slot_record.segment && ${s}")

lookupVolumeContainersRecords :: (MonadDB m) => Volume -> m [(Container, [RecordSlot])]
lookupVolumeContainersRecords v =
  map (second catMaybes) . groupTuplesBy ((==) `on` containerId) <$>
    dbQuery (($ v) <$> $(selectQuery selectVolumeSlotMaybeRecord "WHERE container.volume = ${volumeId v} ORDER BY container.id, record.category NULLS FIRST, slot_record.segment, slot_record.record"))

lookupVolumeContainersRecordIds :: (MonadDB m) => Volume -> m [(Container, [(Segment, Id Record)])]
lookupVolumeContainersRecordIds v =
  map (second catMaybes) . groupTuplesBy ((==) `on` containerId) <$>
    dbQuery (($ v) <$> $(selectQuery selectVolumeSlotMaybeRecordId "$WHERE container.volume = ${volumeId v} ORDER BY container.id, slot_record.segment, slot_record.record"))

moveRecordSlot :: (MonadAudit c m) => RecordSlot -> Segment -> m Bool
moveRecordSlot rs@RecordSlot{ recordSlot = s@Slot{ slotSegment = src } } dst = do
  ident <- getAuditIdentity
  either (const False) id
    <$> case (Range.isEmpty (segmentRange src), Range.isEmpty (segmentRange dst)) of
    (True,  True) -> return $ Right False
    (False, True) -> Right <$> dbExecute1 $(deleteSlotRecord 'ident 'rs)
    (True,  False) -> dbTryJust err $ dbExecute1 $(insertSlotRecord 'ident 'rd)
    (False, False) -> dbTryJust err $ dbExecute1 $(updateSlotRecord 'ident 'rs 'dst)
  where
  rd = rs{ recordSlot = s{ slotSegment = dst } }
  err = guard . isExclusionViolation

recordSlotAge :: RecordSlot -> Maybe Age
recordSlotAge rs@RecordSlot{..} =
  clip <$> liftM2 age (decodeMeasure (PGTypeProxy :: PGTypeName "date") =<< getMeasure birthdateMetric (recordMeasures slotRecord)) (containerDate $ slotContainer recordSlot)
  where
  clip a
    | dataPermission rs == PermissionNONE = a `min` ageLimit
    | otherwise = a
  ageLimit = yearsAge (90 :: Int)

recordSlotJSON :: RecordSlot -> JSON.Object
recordSlotJSON rs@RecordSlot{..} = JSON.record (recordId slotRecord) $ catMaybes
  [ segmentJSON (slotSegment recordSlot)
  , ("age" JSON..=) <$> recordSlotAge rs
  ]
