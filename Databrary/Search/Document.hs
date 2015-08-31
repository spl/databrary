{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings #-}
module Databrary.Search.Document
  ( SolrDocument(..)
  , SolrRecordMeasures(..)
  , SolrSegment(..)
  ) where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.TH as JTH
import qualified Data.ByteString as BS
import Data.Char (isAlphaNum, toLower)
import Data.Int (Int16)
import qualified Data.HashMap.Strict as HM
import Data.Monoid ((<>))
import qualified Data.Text as T

import Databrary.Model.URL
import Databrary.Model.Id.Types
import Databrary.Model.Permission.Types
import Databrary.Model.Release.Types
import Databrary.Model.Party.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Offset
import Databrary.Model.Segment
import Databrary.Model.Format.Types
import Databrary.Model.Asset.Types
import Databrary.Model.Age
import Databrary.Model.Record.Types
import Databrary.Model.Metric.Types
import Databrary.Model.Tag.Types
import Databrary.Search.Util

safeField :: T.Text -> T.Text
safeField = T.map safeChar where
  safeChar c
    | isAlphaNum c = c
    | otherwise = '_'

newtype SolrRecordMeasures = SolrRecordMeasures [(Metric, MeasureDatum)]

metricSuffix :: Metric -> T.Text
metricSuffix Metric{ metricType = MeasureTypeText, metricOptions = [] } = "t"
metricSuffix Metric{ metricType = MeasureTypeText } = "s"
metricSuffix Metric{ metricType = MeasureTypeNumeric } = "td"
metricSuffix Metric{ metricType = MeasureTypeDate } = "tdt"
metricSuffix Metric{ metricType = MeasureTypeVoid } = "b"

metricField :: Metric -> T.Text
metricField m = safeField (metricName m) <> ('_' `T.cons` metricSuffix m)

-- slight hack because we actually index dates as datetimes
metricDatum :: Metric -> MeasureDatum -> JSON.Value
metricDatum Metric{ metricType = MeasureTypeDate } d = JSON.toJSON $ d <> "T12:00:00Z"
metricDatum Metric{ metricType = MeasureTypeVoid } _ = JSON.toJSON True
metricDatum _ d = JSON.toJSON d

instance JSON.ToJSON SolrRecordMeasures where
  toJSON (SolrRecordMeasures ms) =
    JSON.object $ map (\(m, d) -> ("record_" <> metricField m) JSON..= metricDatum m d) ms

newtype SolrSegment = SolrSegment Segment deriving (JSON.FromJSON)

instance JSON.ToJSON SolrSegment where
  toJSON (SolrSegment s) = JSON.String $ T.pack $ showSegmentWith (shows . offsetMillis) s ""

data SolrDocument
  = SolrParty
    { solrId :: !BS.ByteString
    , solrPartyId_i :: Id Party
    , solrPartyName_s :: T.Text
    , solrPartyPreName_s :: Maybe T.Text
    , solrPartyAffiliation_s :: Maybe T.Text
    , solrPartyIsInstitution_b :: Bool
    , solrPartyAuthorization_i :: Maybe Permission
    }
  | SolrVolume
    { solrId :: !BS.ByteString
    , solrVolumeId_i :: Id Volume
    , solrName_t :: Maybe T.Text
    , solrBody_t :: Maybe T.Text -- body
    , solrVolumeOwnerIds_is :: [Id Party]
    , solrVolumeOwnerNames_ss :: [T.Text]
    , solrCitation_t :: Maybe T.Text
    , solrCitationUrl_s :: Maybe URI
    , solrCitationYear_i :: Maybe Int16
    }
  | SolrContainer
    { solrId :: !BS.ByteString
    , solrVolumeId_i :: Id Volume
    , solrContainerId_i :: Id Container
    , solrName_t :: Maybe T.Text
    , solrContainerTop_b :: Bool
    , solrRelease_i :: Maybe Release
    }
  | SolrAsset -- Slot
    { solrId :: !BS.ByteString
    , solrVolumeId_i :: Id Volume
    , solrContainerId_i :: Id Container
    , solrSegment :: SolrSegment
    , solrSegmentDuration_td :: Maybe Offset
    , solrAssetId_i :: Id Asset
    , solrName_t :: Maybe T.Text
    , solrFormat_i :: Id Format
    , solrRelease_i :: Maybe Release
    }
  | SolrExcerpt
    { solrId :: !BS.ByteString
    , solrVolumeId_i :: Id Volume
    , solrContainerId_i :: Id Container
    , solrSegment :: SolrSegment
    , solrSegmentDuration_td :: Maybe Offset
    , solrAssetId_i :: Id Asset
    , solrRelease_i :: Maybe Release
    }
  | SolrRecord -- Slot
    { solrId :: !BS.ByteString
    , solrVolumeId_i :: Id Volume
    , solrContainerId_i :: Id Container
    , solrSegment :: SolrSegment
    , solrSegmentDuration_td :: Maybe Offset
    , solrRecordId_i :: Id Record
    , solrRecordCategory_s :: T.Text
    , solrRecordMeasures :: SolrRecordMeasures
    , solrRecordAge_ti :: Maybe Age
    }
  | SolrTag -- Use
    { solrId :: !BS.ByteString
    , solrVolumeId_i :: Id Volume
    , solrContainerId_i :: Id Container
    , solrSegment :: SolrSegment
    , solrSegmentDuration_td :: Maybe Offset
    , solrTagId_i :: Id Tag
    , solrTag_s :: TagName
    , solrKeyword_s :: Maybe TagName
    , solrPartyId_i :: Id Party
    }

$(return []) -- force new decl group for splice:

solrToJSON :: SolrDocument -> JSON.Value
solrToJSON =
  $(JTH.mkToJSON JTH.defaultOptions
    { JTH.fieldLabelModifier = \('s':'o':'l':'r':c:s) -> toLower c:unCamel s
    , JTH.constructorTagModifier = \('S':'o':'l':'r':c:s) -> toLower c:unCamel s
    , JTH.omitNothingFields = True
    , JTH.sumEncoding = JTH.TaggedObject
      { JTH.tagFieldName = "content_type"
      , JTH.contentsFieldName = error "solrToJSON: contentsFieldName"
      }
    } ''SolrDocument)

fixToJSON :: SolrDocument -> JSON.Value -> JSON.Value
fixToJSON SolrRecord{} (JSON.Object o) = JSON.Object $
  maybe o (HM.union $ HM.delete k o) $ do
    JSON.Object m <- HM.lookup k o
    return m
  where k = "record_measures"
fixToJSON _ j = j

instance JSON.ToJSON SolrDocument where
  toJSON s = fixToJSON s $ solrToJSON s
