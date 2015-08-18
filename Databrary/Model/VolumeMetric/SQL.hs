{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.VolumeMetric.SQL
  ( selectVolumeMetric
  ) where

import Databrary.Model.SQL.Select

selectVolumeMetric :: Selector -- ^ @(Id RecordCategory, Id Metric)@
selectVolumeMetric = selectColumns '(,) "volume_metric" ["category", "metric"]
