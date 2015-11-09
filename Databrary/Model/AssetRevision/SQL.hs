{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.AssetRevision.SQL
  ( selectAssetRevision
  ) where

import qualified Data.Foldable as Fold
import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL.Select
import Databrary.Model.Asset.Types
import Databrary.Model.Asset.SQL
import Databrary.Model.AssetRevision.Types

makeAssetRevision :: Maybe Bool -> Asset -> Asset -> AssetRevision
makeAssetRevision t o a = AssetRevision a o (Fold.or t)

selectAssetRevision :: TH.Name -- ^ @'Identity'@
  -> Selector -- ^ @'Asset' -> 'AssetRevision@
selectAssetRevision ident = selectJoin 'makeAssetRevision
  [ selector "asset_revision" (SelectExpr "asset_revision.tableoid = 'transcode'::regclass")
  , joinOn "asset_revision.orig = asset.id"
    $ selectAsset ident
  ]
