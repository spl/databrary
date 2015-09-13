{-# LANGUAGE OverloadedStrings, RecordWildCards, TemplateHaskell, QuasiQuotes #-}
module Databrary.Model.Asset
  ( module Databrary.Model.Asset.Types
  , blankAsset
  , assetBacked
  , lookupAsset
  , lookupVolumeAsset
  , addAsset
  , changeAsset
  , supersedeAsset
  , assetIsSuperseded
  , assetCreation
  , assetJSON
  ) where

import Control.Arrow (first)
import Data.Maybe (catMaybes, isNothing, isJust)
import qualified Data.Text as T
import Database.PostgreSQL.Typed (pgSQL)

import Databrary.Ops
import Databrary.Has (view, peek)
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Files
import Databrary.Store.Types
import Databrary.Store.Asset
import Databrary.Model.SQL
import Databrary.Model.Time
import Databrary.Model.Audit
import Databrary.Model.Id
import Databrary.Model.Identity
import Databrary.Model.Party
import Databrary.Model.Volume
import Databrary.Model.Format
import Databrary.Model.Asset.Types
import Databrary.Model.Asset.SQL

blankAsset :: Volume -> Asset
blankAsset vol = Asset
  { assetId = error "blankAsset"
  , assetFormat = unknownFormat
  , assetRelease = Nothing
  , assetName = Nothing
  , assetDuration = Nothing
  , assetSHA1 = Nothing
  , assetSize = Nothing
  , assetVolume = vol
  }

assetBacked :: Asset -> Bool
assetBacked = isJust . assetSHA1

lookupAsset :: (MonadHasIdentity c m, MonadDB c m) => Id Asset -> m (Maybe Asset)
lookupAsset ai = do
  ident <- peek
  dbQuery1 $(selectQuery (selectAsset 'ident) "$WHERE asset.id = ${ai}")

lookupVolumeAsset :: (MonadDB c m) => Volume -> Id Asset -> m (Maybe Asset)
lookupVolumeAsset vol ai = do
  dbQuery1 $ ($ vol) <$> $(selectQuery selectVolumeAsset "WHERE asset.id = ${ai} AND asset.volume = ${volumeId vol}")

addAsset :: (MonadAudit c m, MonadStorage c m) => Asset -> Maybe RawFilePath -> m Asset
addAsset ba fp = do
  ident <- getAuditIdentity
  ba' <- maybe (return ba) (storeAssetFile ba) fp
  dbQuery1' $(insertAsset 'ident 'ba')

changeAsset :: (MonadAudit c m, MonadStorage c m) => Asset -> Maybe RawFilePath -> m Asset
changeAsset a fp = do
  ident <- getAuditIdentity
  a2 <- maybe (return a) (storeAssetFile a) fp
  dbExecute1' $(updateAsset 'ident 'a2)
  return a2

supersedeAsset :: MonadDB c m => Asset -> Asset -> m ()
supersedeAsset old new =
  dbExecute1' [pgSQL|SELECT asset_supersede(${assetId old}, ${assetId new})|]

assetIsSuperseded :: MonadDB c m => Asset -> m Bool
assetIsSuperseded a =
  dbExecute1 [pgSQL|SELECT ''::void FROM asset_revision WHERE orig = ${assetId a} LIMIT 1|]

assetCreation :: MonadDB c m => Asset -> m (Maybe Timestamp, Maybe T.Text)
assetCreation a = maybe (Nothing, Nothing) (first Just) <$>
  dbQuery1 [pgSQL|$SELECT audit_time, name FROM audit.asset WHERE id = ${assetId a} AND audit_action = 'add' ORDER BY audit_time DESC LIMIT 1|]

assetJSON :: Asset -> JSON.Object
assetJSON Asset{..} = JSON.record assetId $ catMaybes
  [ Just $ "format" JSON..= formatId assetFormat
  , ("classification" JSON..=) <$> assetRelease
  , ("duration" JSON..=) <$> assetDuration
  , ("pending" JSON..= isNothing assetSize) <? isNothing assetSHA1
  ]
