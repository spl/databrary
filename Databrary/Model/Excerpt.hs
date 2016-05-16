{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards, DataKinds #-}
module Databrary.Model.Excerpt
  ( lookupAssetExcerpts
  , lookupSlotExcerpts
  , lookupVolumeExcerpts
  , lookupSlotThumb
  , lookupVolumeThumb
  , changeExcerpt
  , removeExcerpt
  , excerptJSON
  ) where

import Control.Monad (guard)

import Databrary.Has (view)
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Permission
import Databrary.Model.Audit
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Slot.Types
import Databrary.Model.Asset.Types
import Databrary.Model.AssetSlot.Types
import Databrary.Model.AssetSegment
import Databrary.Model.Excerpt.SQL

lookupAssetExcerpts :: MonadDB c m => AssetSlot -> m [Excerpt]
lookupAssetExcerpts a =
  dbQuery $ ($ a) <$> $(selectQuery selectAssetSlotExcerpt "$WHERE excerpt.asset = ${assetId $ assetRow $ slotAsset a}")

lookupSlotExcerpts :: MonadDB c m => Slot -> m [Excerpt]
lookupSlotExcerpts (Slot c s) =
  dbQuery $ ($ c) <$> $(selectQuery selectContainerExcerpt "$WHERE slot_asset.container = ${containerId $ containerRow c} AND excerpt.segment && ${s}")

lookupVolumeExcerpts :: MonadDB c m => Volume -> m [Excerpt]
lookupVolumeExcerpts v =
  dbQuery $ ($ v) <$> $(selectQuery selectVolumeExcerpt "$WHERE asset.volume = ${volumeId $ volumeRow v}")

lookupSlotThumb :: MonadDB c m => Slot -> m (Maybe AssetSegment)
lookupSlotThumb (Slot c s) = do
  dbQuery1 $ assetSegmentInterp 0 . excerptAsset . ($ c) <$> $(selectQuery selectContainerExcerpt "$\
    \JOIN format ON asset.format = format.id \
    \WHERE slot_asset.container = ${containerId $ containerRow c} AND excerpt.segment && ${s} \
      \AND COALESCE(GREATEST(excerpt.release, asset.release), ${containerRelease c}) >= ${readRelease (view c)}::release \
      \AND (asset.duration IS NOT NULL AND format.mimetype LIKE 'video/%' OR format.mimetype LIKE 'image/%') \
      \AND asset.sha1 IS NOT NULL \
    \LIMIT 1")

lookupVolumeThumb :: MonadDB c m => Volume -> m (Maybe AssetSegment)
lookupVolumeThumb v = do
  dbQuery1 $ assetSegmentInterp 0 . excerptAsset . ($ v) <$> $(selectQuery selectVolumeExcerpt "$\
    \JOIN format ON asset.format = format.id \
    \WHERE asset.volume = ${volumeId $ volumeRow v} \
      \AND COALESCE(GREATEST(excerpt.release, asset.release), slot_release.release) >= ${readRelease (view v)}::release \
      \AND (asset.duration IS NOT NULL AND format.mimetype LIKE 'video/%' OR format.mimetype LIKE 'image/%') \
      \AND asset.sha1 IS NOT NULL \
    \ORDER BY container.top DESC LIMIT 1")

changeExcerpt :: MonadAudit c m => Excerpt -> m Bool
changeExcerpt e = do
  ident <- getAuditIdentity
  either (const False) ((0 <) . fst) <$> tryUpdateOrInsert (guard . isExclusionViolation)
    $(updateExcerpt 'ident 'e)
    $(insertExcerpt 'ident 'e)

removeExcerpt :: MonadAudit c m => AssetSegment -> m Bool
removeExcerpt e = do
  ident <- getAuditIdentity
  dbExecute1 $(deleteExcerpt 'ident 'e)

excerptJSON :: JSON.ToObject o => Excerpt -> o
excerptJSON = assetSegmentJSON . excerptAsset
