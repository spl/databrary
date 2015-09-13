{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
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

import Databrary.Ops
import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Model.RecordCategory
import Databrary.Model.Metric
import Databrary.Model.VolumeMetric.SQL

lookupVolumeMetrics :: (MonadDB c m) => Volume -> m [(Id RecordCategory, [Id Metric])]
lookupVolumeMetrics v =
  {- map (getRecordCategory' *** map getMetric') . -} groupTuplesBy (==) <$>
  dbQuery $(selectQuery selectVolumeMetric "$WHERE volume = ${volumeId v} ORDER BY category, metric")

addVolumeCategory :: (MonadDB c m) => Volume -> Id RecordCategory -> m [Id Metric]
addVolumeCategory v c =
  dbQuery [pgSQL|INSERT INTO volume_metric SELECT ${volumeId v}, category, metric FROM record_template WHERE category = ${c} RETURNING metric|]
  
addVolumeMetric :: (MonadDB c m) => Volume -> Id RecordCategory -> Id Metric -> m Bool
addVolumeMetric v c m = liftDBM $
  handleJust (guard . isUniqueViolation) (const $ return False) $
    dbExecute1 [pgSQL|INSERT INTO volume_metric VALUES (${volumeId v}, ${c}, ${m})|]

removeVolumeMetric :: (MonadDB c m) => Volume -> Id RecordCategory -> Id Metric -> m Bool
removeVolumeMetric v c m =
  dbExecute1 [pgSQL|DELETE FROM volume_metric WHERE volume = ${volumeId v} AND category = ${c} AND metric = ${m}|]

removeVolumeCategory :: (MonadDB c m) => Volume -> Id RecordCategory -> m Int
removeVolumeCategory v c =
  dbExecute [pgSQL|DELETE FROM volume_metric WHERE volume = ${volumeId v} AND category = ${c}|]
