{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Model.Asset.SQL
  ( selectVolumeAsset
  , selectAsset
  , insertAsset
  , updateAsset
  ) where

import qualified Data.ByteString as BS
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Language.Haskell.TH as TH

import Databrary.Model.Offset
import Databrary.Model.Format
import Databrary.Model.Id.Types
import Databrary.Model.Release.Types
import Databrary.Model.SQL.Select
import Databrary.Model.Audit.SQL
import Databrary.Model.Volume.Types
import Databrary.Model.Volume.SQL
import Databrary.Model.Asset.Types

makeAsset :: Id Asset -> Id Format -> Maybe Release -> Maybe T.Text -> Maybe Offset -> Maybe BS.ByteString -> Maybe Int64 -> Volume -> Asset
makeAsset i = Asset i . getFormat'

assetRow :: Selector -- ^ @'Volume' -> 'Asset'@
assetRow = selectColumns 'makeAsset "asset" ["id", "format", "release", "name", "duration", "sha1", "size"]

selectVolumeAsset :: Selector -- ^ @'Volume' -> 'Asset'@
selectVolumeAsset = assetRow

selectAsset :: TH.Name -- ^ @'Identity'@
  -> Selector -- ^ @'Asset'@
selectAsset ident = selectJoin '($)
  [ selectVolumeAsset
  , joinOn "asset.volume = volume.id" $ selectVolume ident
  ]

assetKeys :: String -- ^ @'Asset'@
  -> [(String, String)]
assetKeys r =
  [ ("id", "${assetId " ++ r ++ "}") ]

assetSets :: String -- ^ @'Asset'@
  -> [(String, String)]
assetSets a =
  [ ("volume", "${volumeId (assetVolume " ++ a ++ ")}")
  , ("format", "${formatId (assetFormat " ++ a ++ ")}")
  , ("release", "${assetRelease " ++ a ++ "}")
  , ("duration", "${assetDuration " ++ a ++ "}")
  , ("name", "${assetName " ++ a ++ "}")
  , ("sha1", "${assetSHA1 " ++ a ++ "}")
  , ("size", "${assetSize " ++ a ++ "}")
  ]

setAssetId :: Asset -> Id Asset -> Asset
setAssetId a i = a{ assetId = i }

insertAsset :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Asset'@
  -> TH.ExpQ -- ^ @'Asset'@
insertAsset ident a = auditInsert ident "asset"
  (assetSets (nameRef a))
  (Just $ selectOutput $ selectMap ((TH.VarE 'setAssetId `TH.AppE` TH.VarE a) `TH.AppE`) $ selector "asset" $ SelectColumn "asset" "id")

updateAsset :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Asset'@
  -> TH.ExpQ -- ^ @()@
updateAsset ident a = auditUpdate ident "asset"
  (assetSets (nameRef a))
  (whereEq $ assetKeys (nameRef a))
  Nothing
