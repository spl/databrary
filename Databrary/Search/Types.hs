{-# LANGUAGE TemplateHaskell #-}
module Databrary.Search.Types
  ( SolrDocument(..)
  ) where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.TH as JTH
import qualified Data.ByteString as BS
import Data.Char (isUpper, toLower)
import Data.Int (Int16)
import qualified Data.Text as T

import Databrary.Model.URL
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Record.Types
import Databrary.Model.Metric.Types
import Databrary.Model.Age
import Databrary.Model.Tag.Types

newtype SolrRecordMeasures = SolrRecordMeasures [(Metric, MeasureDatum)]

metricSuffix :: Metric -> T.Text
metricSuffix Metric{ metricType = MeasureTypeText, metricOptions = [] } = "_t"
metricSuffix Metric{ metricType = MeasureTypeText } = "_s"
metricSuffix Metric{ metricType = MeasureTypeNumeric } = "_td"
metricSuffix Metric{ metricType = MeasureTypeDate } = "_tdt"

instance JSON.ToJSON SolrRecordMeasures where
  toJSON (SolrRecordMeasures ms) =
    JSON.object $ map (\(m, d) -> ("record_" <> metricName m <> '_' `T.cons` metricSuffix m, d)) ms

data SolrDocument
  = SolrVolume
    { solrId :: BS.ByteString
    , solrVolumeId_i :: Id Volume
    , solrTitle_t :: T.Text
    , solrAbs_t :: T.Text -- body
    , solrAlias_s :: Maybe T.Text
    , solrVolumeOwnerIds_is :: [Id Party]
    , solrVolumeOwnerNames_ss :: [T.Text]
    , solrCitation_t :: Maybe T.Text
    , solrCitationUrl_s :: Maybe URI
    , solrCitationYear_i :: Maybe Int16
    , solrVolumeHasSessions_b :: Bool
    , solrHasExcerpt_b :: Bool
    , solrKeywords_ss :: [TagName]
    , solrTags_ss :: [TagName]
    }
  | SolrContainer
    { solrId :: BS.ByteString
    , solrVolumeId_i :: Id Volume
    , solrContainerId_i :: Id Container
    , solrContainerName_t :: Maybe T.Text
    , solrHasExcerpt_b :: Bool
    , solrKeywords_ss :: [TagName]
    , solrTags_ss :: [TagName]
    }
  | SolrSlotRecord
    { solrId :: BS.ByteString
    , solrVolumeId_i :: Id Volume
    , solrContainerId_i :: Id Container
    , solrRecordId_i :: Id Record
    , solrRecordMeasures :: SolrRecordMeasures
    , solrRecordAge_td :: Maybe Age
    }

solrToJSON :: SolrDocument -> JSON.Value
solrToJSON =
  $(let uncamel "" = ""
        uncamel (c:s)
          | isUpper c = '_':toLower c:uncamel s
          | otherwise = c:uncamel s
  in JTH.mkToJSON JTH.defaultOptions
    { JTH.fieldLabelModifier = \('s':'o':'l':'r':c:s) -> toLower c:uncamel s
    , JTH.constructorTagModifier = \('S':'o':'l':'r':c:s) -> toLower c:uncamel s
    , JTH.omitNothingFields = True
    , JTH.sumEncoding = JTH.TaggedObject
      { JTH.tagFieldName = "content_type"
      , JTH.contentsFieldName = error "contentsFieldName"
      }
    } ''SolrDocument)

fixToJSON :: SolrDocument -> JSON.Value -> JSON.Value
fixToJSON (SolrSlotRecord _) (JSON.Object o) = JSON.Object $
  maybe o (union $ delete k o) $ do
    JSON.Object m <- HM.lookup k o
    return m
  where k = "record_measures"
fixToJSON _ j = j

instance JSON.ToJSON SolrDocument where
  toJSON s = fixToJSON s $ solrToJSON s
