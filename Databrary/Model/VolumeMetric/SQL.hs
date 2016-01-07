{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.VolumeMetric.SQL
  ( selectVolumeMetric
  ) where

import Databrary.Model.SQL.Select

selectVolumeMetric :: Selector -- ^ @Metric@
selectVolumeMetric = selectColumns 'id "volume_metric" ["metric"]
