{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Model.AssetSlot.SQL
  ( slotAssetRow
  , makeSlotAsset
  , selectContainerSlotAsset
  , selectAssetSlotAsset
  , selectVolumeSlotAsset
  , selectVolumeSlotIdAsset
  , selectSlotAsset
  , selectAssetSlot
  , insertSlotAsset
  , updateSlotAsset
  , deleteSlotAsset
  ) where

import Data.Maybe (fromMaybe)
import qualified Language.Haskell.TH as TH

import Databrary.Has (view)
import Databrary.Model.Segment
import Databrary.Model.Volume.Types
import Databrary.Model.Asset.Types
import Databrary.Model.Asset.SQL
import Databrary.Model.Container.Types
import Databrary.Model.Container.SQL
import Databrary.Model.Slot.Types
import Databrary.Model.SQL.Select
import Databrary.Model.Audit.SQL
import Databrary.Model.Volume.SQL
import Databrary.Model.AssetSlot.Types

slotAssetRow :: Selector -- ^ @'Segment'@
slotAssetRow = selector "slot_asset" $ SelectColumn "slot_asset" "segment"

makeSlotAsset :: Asset -> Container -> Segment -> AssetSlot
makeSlotAsset a c s = AssetSlot a (Just (Slot c s))

_selectAssetContainerSlotAsset :: Selector -- ^ @'Asset' -> 'Container' -> 'AssetSlot'@
_selectAssetContainerSlotAsset = selectMap (TH.VarE 'makeSlotAsset `TH.AppE`) slotAssetRow

makeContainerSlotAsset :: Segment -> (Volume -> Asset) -> Container -> AssetSlot
makeContainerSlotAsset s af c = makeSlotAsset (af (view c)) c s

selectContainerSlotAsset :: Selector -- ^ @'Container' -> 'AssetSlot'@
selectContainerSlotAsset = selectJoin 'makeContainerSlotAsset
  [ slotAssetRow
  , joinOn "slot_asset.asset = asset.id"
    selectVolumeAsset -- XXX volumes match?
  ]

makeVolumeSlotIdAsset :: SlotId -> (Volume -> Asset) -> Volume -> (Asset, SlotId)
makeVolumeSlotIdAsset s af v = (af v, s)

selectVolumeSlotIdAsset :: Selector -- ^ @'Volume' -> ('Asset', 'SlotId')@
selectVolumeSlotIdAsset = selectJoin 'makeVolumeSlotIdAsset
  [ selectColumns 'SlotId "slot_asset" ["container", "segment"]
  , joinOn "slot_asset.asset = asset.id"
    selectVolumeAsset -- XXX volumes match?
  ]

makeAssetSlotAsset :: Segment -> (Volume -> Container) -> Asset -> AssetSlot
makeAssetSlotAsset s cf a = makeSlotAsset a (cf (view a)) s

selectAssetSlotAsset :: Selector -- ^ @'Asset' -> 'AssetSlot'@
selectAssetSlotAsset = selectJoin 'makeAssetSlotAsset
  [ slotAssetRow
  , joinOn "slot_asset.container = container.id"
    selectVolumeContainer -- XXX volumes match?
  ]

makeVolumeSlotAsset :: Segment -> (Volume -> Asset) -> (Volume -> Container) -> Volume -> AssetSlot
makeVolumeSlotAsset s af cf v = makeSlotAsset (af v) (cf v) s

selectVolumeSlotAsset :: Selector -- ^ @'Volume' -> 'AssetSlot'@
selectVolumeSlotAsset = selectJoin 'makeVolumeSlotAsset
  [ slotAssetRow
  , joinOn "slot_asset.asset = asset.id"
    selectVolumeAsset
  , joinOn "slot_asset.container = container.id AND asset.volume = container.volume"
    selectVolumeContainer
  ]

selectSlotAsset :: TH.Name -- ^ @'Identity'@
  -> Selector -- ^ @'AssetSlot'@
selectSlotAsset ident = selectJoin '($)
  [ selectVolumeSlotAsset
  , joinOn "asset.volume = volume.id"
    $ selectVolume ident
  ]

makeVolumeAssetSlot :: (Volume -> Asset) -> Maybe (Asset -> AssetSlot) -> Volume -> AssetSlot
makeVolumeAssetSlot af sf = fromMaybe assetNoSlot sf . af

selectVolumeAssetSlot :: Selector -- ^ @'Volume' -> 'AssetSlot'@
selectVolumeAssetSlot = selectJoin 'makeVolumeAssetSlot
  [ selectVolumeAsset
  , maybeJoinOn "asset.id = slot_asset.asset AND asset.volume = container.volume"
    selectAssetSlotAsset
  ]

selectAssetSlot :: TH.Name -- ^ @'Identity'@
  -> Selector -- ^ @'AssetSlot'@
selectAssetSlot ident = selectJoin '($)
  [ selectVolumeAssetSlot
  , joinOn "asset.volume = volume.id"
    $ selectVolume ident
  ]

slotAssetKeys :: String -- ^ @'AssetSlot'@
  -> [(String, String)]
slotAssetKeys as =
  [ ("asset", "${assetId (slotAsset " ++ as ++ ")}") ]

slotAssetSets :: String -- ^ @'AssetSlot'@
  -> [(String, String)]
slotAssetSets as =
  [ ("container", "${containerId . slotContainer <$> assetSlot " ++ as ++ "}")
  , ("segment", "${slotSegment <$> assetSlot " ++ as ++ "}")
  ]

insertSlotAsset :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'AssetSlot'@
  -> TH.ExpQ
insertSlotAsset ident o = auditInsert ident "slot_asset"
  (slotAssetKeys os ++ slotAssetSets os)
  Nothing
  where os = nameRef o

updateSlotAsset :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'AssetSlot'@
  -> TH.ExpQ
updateSlotAsset ident o = auditUpdate ident "slot_asset"
  (slotAssetSets os)
  (whereEq $ slotAssetKeys os)
  Nothing
  where os = nameRef o

deleteSlotAsset :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'AssetSlot'@
  -> TH.ExpQ
deleteSlotAsset ident o = auditDelete ident "slot_asset"
  (whereEq $ slotAssetKeys os)
  Nothing
  where os = nameRef o
