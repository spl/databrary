{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Search.Types
  ( SolrDocument(..)
  ) where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.TH as JTH
import qualified Data.ByteString as BS
import Data.Char (isAlphaNum, isUpper, toLower)
import Data.Int (Int16)
import qualified Data.HashMap.Strict as HM
import Data.Monoid ((<>))
import qualified Data.Text as T

import Databrary.Model.URL
import Databrary.Model.Id.Types
import Databrary.Model.Release.Types
import Databrary.Model.Party.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Asset.Types
import Databrary.Model.Record.Types
import Databrary.Model.Metric.Types
import Databrary.Model.Age
import Databrary.Model.Tag.Types

safeField :: T.Text -> T.Text
safeField = T.map safeChar where
  safeChar c
    | isAlphaNum c = c
    | otherwise = '_'

newtype SolrRecordMeasures = SolrRecordMeasures [(Metric, MeasureDatum)]

metricSuffix :: Metric -> T.Text
metricSuffix Metric{ metricType = MeasureTypeText, metricOptions = [] } = "_t"
metricSuffix Metric{ metricType = MeasureTypeText } = "_s"
metricSuffix Metric{ metricType = MeasureTypeNumeric } = "_td"
metricSuffix Metric{ metricType = MeasureTypeDate } = "_tdt"

metricField :: Metric -> T.Text
metricField m = safeField (metricName m) <> ('_' `T.cons` metricSuffix m)

instance JSON.ToJSON SolrRecordMeasures where
  toJSON (SolrRecordMeasures ms) =
    JSON.object $ map (\(m, d) -> ("record_" <> metricField m) JSON..= d) ms

data SolrDocument
  = SolrParty
    { solrId :: !BS.ByteString
    , solrPartyId_i :: Id Party
    , solrPartyName_s :: T.Text
    , solrPartyPreName_s :: Maybe T.Text
    , solrPartyAffiliation_s :: Maybe T.Text
    , solrPartyIsInstitution_b :: Bool
    -- TODO: authorization...
    }
  | SolrVolume
    { solrId :: !BS.ByteString
    , solrVolumeId_i :: Id Volume
    , solrName_t :: Maybe T.Text
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
    { solrId :: !BS.ByteString
    , solrVolumeId_i :: Id Volume
    , solrContainerId_i :: Id Container
    , solrName_t :: Maybe T.Text
    , solrRelease_i :: Maybe Release
    , solrHasExcerpt_b :: Bool
    , solrKeywords_ss :: [TagName]
    , solrTags_ss :: [TagName]
    }
  | SolrAsset -- Slot
    { solrId :: !BS.ByteString
    , solrVolumeId_i :: Id Volume
    , solrContainerId_i :: Id Container
    , solrAssetId_i :: Id Asset
    , solrName_t :: Maybe T.Text
    , solrRelease_i :: Maybe Release
    -- TODO: format? (duration?) segment...
    }
  | SolrRecord -- Slot
    { solrId :: !BS.ByteString
    , solrVolumeId_i :: Id Volume
    , solrContainerId_i :: Id Container
    , solrRecordId_i :: Id Record
    , solrRecordMeasures :: SolrRecordMeasures
    , solrRecordAge_td :: Maybe Age
    -- TODO: segment...
    }

$(return []) -- force new decl group for splice:

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
fixToJSON SolrRecord{} (JSON.Object o) = JSON.Object $
  maybe o (HM.union $ HM.delete k o) $ do
    JSON.Object m <- HM.lookup k o
    return m
  where k = "record_measures"
fixToJSON _ j = j

instance JSON.ToJSON SolrDocument where
  toJSON s = fixToJSON s $ solrToJSON s
