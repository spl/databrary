{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Databrary.Model.AssetRevision
  ( module Databrary.Model.AssetRevision.Types
  , supersedeAsset
  , assetIsSuperseded
  , lookupAssetRevision
  ) where

import Database.PostgreSQL.Typed.Query (pgSQL)

import Databrary.Ops
import Databrary.Has
import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Id
import Databrary.Model.Party
import Databrary.Model.Identity
import Databrary.Model.Asset
import Databrary.Model.AssetRevision.Types
import Databrary.Model.AssetRevision.SQL

useTDB

supersedeAsset :: MonadDB c m => Asset -> Asset -> m ()
supersedeAsset old new =
  dbExecute1' [pgSQL|SELECT asset_supersede(${assetId old}, ${assetId new})|]

assetIsSuperseded :: MonadDB c m => Asset -> m Bool
assetIsSuperseded a =
  dbExecute1 [pgSQL|SELECT ''::void FROM asset_revision WHERE orig = ${assetId a} LIMIT 1|]

lookupAssetRevision :: (MonadHasIdentity c m, MonadDB c m) => Asset -> m (Maybe AssetRevision)
lookupAssetRevision a = do
  ident <- peek
  dbQuery1 $ ($ a) <$> $(selectQuery (selectAssetRevision 'ident) "$WHERE asset_revision.asset = ${assetId a}")
