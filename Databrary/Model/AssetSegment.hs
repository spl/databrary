{-# LANGUAGE TemplateHaskell, QuasiQuotes, RecordWildCards, OverloadedStrings #-}
module Databrary.Model.AssetSegment
  ( module Databrary.Model.AssetSegment.Types
  , lookupAssetSegment
  , lookupSlotAssetSegment
  , lookupAssetSlotSegment
  , auditAssetSegmentDownload
  , assetSegmentJSON
  , assetSegmentInterp
  ) where

import Data.Maybe (catMaybes)
import Database.PostgreSQL.Typed (pgSQL)

import Databrary.Ops
import Databrary.Has (peek, view)
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Audit
import Databrary.Model.Id
import Databrary.Model.Party.Types
import Databrary.Model.Identity
import Databrary.Model.Permission
import Databrary.Model.Segment
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Slot.Types
import Databrary.Model.Format.Types
import Databrary.Model.Asset.Types
import Databrary.Model.AssetSlot
import Databrary.Model.AssetSegment.Types
import Databrary.Model.AssetSegment.SQL

lookupAssetSegment :: (MonadHasIdentity c m, MonadDB m) => Segment -> Id Asset -> m (Maybe AssetSegment)
lookupAssetSegment seg ai = do
  ident :: Identity <- peek
  dbQuery1 $(selectQuery (selectAssetSegment 'ident 'seg) "$WHERE slot_asset.asset = ${ai} AND slot_asset.segment && ${seg}")

lookupSlotAssetSegment :: (MonadHasIdentity c m, MonadDB m) => Id Slot -> Id Asset -> m (Maybe AssetSegment)
lookupSlotAssetSegment (Id (SlotId ci seg)) ai = do
  ident :: Identity <- peek
  dbQuery1 $(selectQuery (selectAssetSegment 'ident 'seg) "$WHERE slot_asset.container = ${ci} AND slot_asset.asset = ${ai} AND slot_asset.segment && ${seg}")

lookupAssetSlotSegment :: MonadDB m => AssetSlot -> Segment -> m (Maybe AssetSegment)
lookupAssetSlotSegment a s =
  segmentEmpty seg ?!$> as <$>
    dbQuery1 $(selectQuery excerptRow "$WHERE asset = ${view a :: Id Asset} AND segment @> ${seg}")
  where
  as = makeExcerpt a s
  seg = assetSegment $ as Nothing

auditAssetSegmentDownload :: MonadAudit c m => Bool -> AssetSegment -> m ()
auditAssetSegmentDownload success AssetSegment{ segmentAsset = AssetSlot{ slotAsset = a, assetSlot = as }, assetSegment = seg } = do
  ai <- getAuditIdentity
  maybe
    (dbExecute1' [pgSQL|INSERT INTO audit.asset (audit_action, audit_user, audit_ip, id, volume, format, release) VALUES
      (${act}, ${auditWho ai}, ${auditIp ai}, ${assetId a}, ${volumeId $ assetVolume a}, ${formatId $ assetFormat a}, ${assetRelease a})|])
    (\s -> dbExecute1' [pgSQL|$INSERT INTO audit.slot_asset (audit_action, audit_user, audit_ip, container, segment, asset) VALUES
      (${act}, ${auditWho ai}, ${auditIp ai}, ${containerId $ slotContainer s}, ${seg}, ${assetId a})|])
    as
  where act | success = AuditActionOpen 
            | otherwise = AuditActionAttempt

assetSegmentJSON :: AssetSegment -> JSON.Object
assetSegmentJSON as@AssetSegment{..}
  | assetSegmentFull as = assetSlotJSON segmentAsset JSON..++ catMaybes fields
  | otherwise = JSON.object $ catMaybes $
    [ Just $ "asset" JSON..= assetSegmentJSON (assetFullSegment as)
    , Just $ ("segment" JSON..= assetSegment)
    , view segmentAsset == fmt ?!> "format" JSON..= formatId fmt
    ] ++ fields
  where
  fields =
    [ -- ("release" JSON..=) <$> (view as :: Maybe Release)
      Just $ "permission" JSON..= dataPermission as
    , ("excerpt" JSON..=) . excerptRelease <$> assetExcerpt
    ]
  fmt = view as

assetSegmentInterp :: Float -> AssetSegment -> AssetSegment
assetSegmentInterp f as = as{ assetSegment = segmentInterp f (assetSegment as) }
