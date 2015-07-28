{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Databrary.Model.VolumeMetric
  ( lookupVolumeMetrics
  , addVolumeTemplateMetrics
  , addVolumeMetric
  , removeVolumeMetric
  ) where

import Control.Arrow ((***))
import Control.Exception.Lifted (handleJust)
import Control.Monad (guard)
import Database.PostgreSQL.Typed.Query (pgSQL)

import Databrary.Ops
import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Volume.Types
import Databrary.Model.RecordCategory
import Databrary.Model.Metric
import Databrary.Model.VolumeMetric.SQL

useTPG

lookupVolumeMetrics :: (MonadDB m) => Volume -> m [(RecordCategory, [Metric])]
lookupVolumeMetrics v =
  map (getRecordCategory' *** map getMetric') . groupTuplesBy (==) <$>
  dbQuery $(selectQuery selectVolumeMetric "$WHERE volume = ${volumeId v} ORDER BY category, metric")

addVolumeTemplateMetrics :: (MonadDB m) => Volume -> RecordCategory -> m Int
addVolumeTemplateMetrics v c =
  dbExecute [pgSQL|INSERT INTO volume_metric SELECT ${volumeId v}, category, metric FROM record_template WHERE category = ${recordCategoryId c}|]
  
addVolumeMetric :: (MonadDB m) => Volume -> RecordCategory -> Metric -> m Bool
addVolumeMetric v c m = liftDBM $
  handleJust (guard . isUniqueViolation) (const $ return False) $
    dbExecute1 [pgSQL|INSERT INTO volume_metric VALUES (${volumeId v}, ${recordCategoryId c}, ${metricId m})|]

removeVolumeMetric :: (MonadDB m) => Volume -> RecordCategory -> Metric -> m Bool
removeVolumeMetric v c m =
  dbExecute1 [pgSQL|DELETE FROM volume_metric WHERE volume = ${volumeId v} AND category = ${recordCategoryId c} AND metric = ${metricId m}|]
