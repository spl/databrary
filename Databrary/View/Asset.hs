{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Asset
  ( htmlAssetEdit
  ) where

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>), mempty)

import Databrary.Action.Auth
import Databrary.Action
import Databrary.Model.Volume
import Databrary.Model.Asset
import Databrary.Model.Container
import Databrary.Model.Slot
import Databrary.Model.AssetSlot
import Databrary.View.Form

import {-# SOURCE #-} Databrary.Controller.Asset

htmlAssetForm :: Maybe Asset -> FormHtml f
htmlAssetForm asset = do
  field "name" $ inputText (assetName =<< asset)
  field "classification" $ inputEnum True (assetRelease =<< asset)
  field "file" inputFile
  -- TODO

htmlAssetEdit :: AssetTarget -> AuthRequest -> FormHtml f
htmlAssetEdit (AssetTargetVolume v) = htmlForm "Create asset" createAsset     (HTML, volumeId v) (htmlAssetForm Nothing) (const mempty)
htmlAssetEdit (AssetTargetSlot s)   = htmlForm "Create asset" createSlotAsset (HTML, slotId s) (field "container" (inputHidden $ show $ containerId $ slotContainer s) >> htmlAssetForm Nothing) (const mempty)
htmlAssetEdit (AssetTargetAsset t)  = htmlForm ("Edit asset " <> fromMaybe "" (assetName a)) postAsset (HTML, assetId a) (htmlAssetForm (Just a)) (const mempty)
  where a = slotAsset t
