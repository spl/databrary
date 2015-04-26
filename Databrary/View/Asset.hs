{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Asset
  ( htmlAssetForm
  ) where

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))

import Databrary.Action.Auth
import Databrary.Action
import Databrary.Model.Volume
import Databrary.Model.Asset
import Databrary.Model.Container
import Databrary.Model.Slot
import Databrary.Model.AssetSlot
import Databrary.View.Form

import {-# SOURCE #-} Databrary.Controller.Asset

htmlAssetForm :: AssetTarget -> AuthRequest -> FormHtml
htmlAssetForm targ req = htmlForm
  (maybe "Create asset" (("Edit asset " <>) . fromMaybe "" . assetName) asset)
  (case targ of
    AssetTargetVolume v -> createAsset HTML $ volumeId v
    AssetTargetSlot s -> createSlotAsset HTML $ slotId s
    AssetTargetAsset a -> postAsset HTML $ assetId $ slotAsset a)
  req
  $ do
  field "name" $ inputText (assetName =<< asset)
  field "classification" $ inputEnum (assetRelease =<< asset)
  field "file" inputFile
  field "container" $ inputHidden (case targ of
    AssetTargetSlot s -> show $ containerId $ slotContainer s
    _ -> "")
  -- TODO
  where
  asset = case targ of
    AssetTargetAsset a -> Just $ slotAsset a
    _ -> Nothing