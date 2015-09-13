{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards, DataKinds #-}
module Databrary.Model.Metric
  ( module Databrary.Model.Metric.Types
  , allMetrics
  , getMetric
  , getMetric'
  , metricLong
  , birthdateMetric
  , metricJSON
  ) where

import qualified Data.IntMap.Strict as IntMap
import Data.List (find)
import Data.Maybe (catMaybes, fromJust)

import Databrary.Ops
import qualified Databrary.JSON as JSON
import Databrary.Model.Id
import Databrary.Model.Metric.Types
import Databrary.Model.Metric.Boot

allMetrics :: [Metric]
allMetrics = $(loadMetrics)

metricsById :: IntMap.IntMap Metric
metricsById = IntMap.fromAscList $ map (\a -> (fromIntegral $ unId $ metricId a, a)) allMetrics

getMetric :: Id Metric -> Maybe Metric
getMetric (Id i) = IntMap.lookup (fromIntegral i) metricsById

getMetric' :: Id Metric -> Metric
getMetric' (Id i) = metricsById IntMap.! fromIntegral i

-- this is a hack, should be in database
metricLong :: Metric -> Bool
metricLong = ("description" ==) . metricName

birthdateMetric :: Metric--T MeasureTypeDate
birthdateMetric = fromJust $ {- castMetric =<< -} find (("birthdate" ==) . metricName) allMetrics

metricJSON :: Metric -> JSON.Object
metricJSON m@Metric{..} = JSON.record metricId $ catMaybes
  [ Just $ "name" JSON..= metricName
  , ("release" JSON..=) <$> metricRelease
  , Just $ "type" JSON..= show metricType
  , "options" JSON..= metricOptions <!? null metricOptions
  , ("assumed" JSON..=) <$> metricAssumed
  , "long" JSON..= True <? metricLong m
  , ("description" JSON..=) <$> metricDescription
  ]
