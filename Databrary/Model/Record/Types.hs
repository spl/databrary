{-# LANGUAGE TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Databrary.Model.Record.Types
  ( Record(..)
  , MonadHasRecord
  , Measure(..)
  , Measures
  ) where

import Control.Applicative ((<|>))

import Databrary.Has (makeHasRec, Has(..))
import Databrary.Model.Kind
import Databrary.Model.Id.Types
import Databrary.Model.Permission.Types
import Databrary.Model.Release.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Metric.Types
import Databrary.Model.RecordCategory.Types

type instance IdType Record = Int32

data Record = Record
  { recordId :: Id Record
  , recordVolume :: Volume
  , recordCategory :: RecordCategory
  , recordRelease :: Maybe Release
  , recordMeasures :: Measures
  }

instance Kinded Record where
  kindOf _ = "record"

data Measure = Measure
  { measureRecord :: Record 
  , measureMetric :: Metric
  , measureDatum :: !MeasureDatum
  }

instance Kinded Measure where
  kindOf _ = "measure"

type Measures = [Measure]

makeHasRec ''Record ['recordId, 'recordVolume, 'recordCategory, 'recordRelease]

instance Has Record Measure where
  view = measureRecord
instance Has (Id Record) Measure where
  view = view . measureRecord
instance Has Volume Measure where
  view = view . measureRecord
instance Has (Id Volume) Measure where
  view = view . measureRecord
instance Has RecordCategory Measure where
  view = view . measureRecord
instance Has (Id RecordCategory) Measure where
  view = view . measureRecord
instance Has Permission Measure where
  view = view . measureRecord

instance Has Metric Measure where
  view = measureMetric
instance Has (Id Metric) Measure where
  view = view . measureMetric
instance Has MeasureType Measure where
  view = view . measureMetric

instance Has (Maybe Release) Measure where
  view m = metricRelease (measureMetric m) <|> recordRelease (measureRecord m)
instance Has Release Measure where
  view = view . (view :: Measure -> Maybe Release)
