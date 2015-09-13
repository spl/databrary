{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable, OverloadedStrings, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Metric.Types
  ( MeasureDatum
  , MeasureType(..)
  , Metric(..)
  , MonadHasMetric
  ) where

import qualified Data.ByteString as BS
import Data.Function (on)
import Data.Ord (comparing)
import qualified Data.Text as T
import Instances.TH.Lift ()
import Language.Haskell.TH.Lift (deriveLiftMany)

import Databrary.Has (makeHasRec)
import Databrary.Model.Enum
import Databrary.Model.Kind
import Databrary.Model.Release.Types
import Databrary.Model.Id.Types

makeDBEnum "data_type" "MeasureType"

type MeasureDatum = BS.ByteString

type instance IdType Metric = Int32

data Metric = Metric
  { metricId :: Id Metric
  , metricName :: T.Text
  , metricRelease :: Maybe Release
  , metricType :: MeasureType
  , metricOptions :: [MeasureDatum]
  , metricAssumed :: Maybe MeasureDatum
  , metricDescription :: Maybe T.Text
  }

instance Kinded Metric where
  kindOf _ = "metric"

instance Eq Metric where
  (==) = on (==) metricId
  (/=) = on (/=) metricId

instance Ord Metric where
  compare = comparing metricId

makeHasRec ''Metric ['metricId, 'metricRelease, 'metricType]
deriveLiftMany [''MeasureType, ''Metric]
