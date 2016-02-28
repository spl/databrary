{-# LANGUAGE TemplateHaskell, QuasiQuotes, DataKinds #-}
module Databrary.Model.VolumeMetric
  ( lookupVolumeMetrics
  , addVolumeCategory
  , addVolumeMetric
  , removeVolumeMetric
  , removeVolumeCategory
  ) where

import Control.Exception.Lifted (handleJust)
import Control.Monad (guard)
import Database.PostgreSQL.Typed.Query (pgSQL)

import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Category
import Databrary.Model.Metric
import Databrary.Model.VolumeMetric.SQL

lookupVolumeMetrics :: (MonadDB c m) => Volume -> m [Id Metric]
lookupVolumeMetrics v =
  dbQuery $(selectQuery selectVolumeMetric "$WHERE volume = ${volumeId $ volumeRow v} ORDER BY metric")

addVolumeCategory :: (MonadDB c m) => Volume -> Id Category -> m [Id Metric]
addVolumeCategory v c =
  dbQuery [pgSQL|INSERT INTO volume_metric SELECT ${volumeId $ volumeRow v}, id FROM metric WHERE category = ${c} AND required IS NOT NULL RETURNING metric|]

addVolumeMetric :: (MonadDB c m) => Volume -> Id Metric -> m Bool
addVolumeMetric v m = liftDBM $
  handleJust (guard . isUniqueViolation) (const $ return False) $
    dbExecute1 [pgSQL|INSERT INTO volume_metric VALUES (${volumeId $ volumeRow v}, ${m})|]

removeVolumeMetric :: (MonadDB c m) => Volume -> Id Metric -> m Bool
removeVolumeMetric v m =
  dbExecute1 [pgSQL|DELETE FROM volume_metric WHERE volume = ${volumeId $ volumeRow v} AND metric = ${m}|]

removeVolumeCategory :: (MonadDB c m) => Volume -> Id Category -> m Int
removeVolumeCategory v c =
  dbExecute [pgSQL|DELETE FROM volume_metric USING metric WHERE volume = ${volumeId $ volumeRow v} AND metric = id AND category = ${c}|]
