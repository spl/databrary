{-# LANGUAGE OverloadedStrings, RecordWildCards, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Databrary.Search.Index
  ( updateIndex
  ) where

import Control.Applicative (Applicative)
import Control.Exception (handle)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT(..))
import Control.Monad.Trans.Class (lift)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Foldable as Fold
import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types.Header (hContentType)

import Control.Invert
import Databrary.Ops
import Databrary.Has (Has(..), makeHasRec)
import Databrary.Service.Types
import Databrary.Service.DB
import Databrary.Service.Log
import Databrary.Model.Time
import Databrary.Model.Segment
import Databrary.Model.Kind
import Databrary.Model.Id.Types
import Databrary.Model.Permission.Types
import Databrary.Model.Identity.Types
import Databrary.Model.Party
import Databrary.Model.Volume.Types
import Databrary.Model.Citation
import Databrary.Model.Container
import Databrary.Model.Slot.Types
import Databrary.Model.Format.Types
import Databrary.Model.Asset.Types
import Databrary.Model.AssetSlot
import Databrary.Model.AssetSegment.Types
import Databrary.Model.Excerpt
import Databrary.Model.RecordCategory.Types
import Databrary.Model.Record.Types
import Databrary.Model.RecordSlot
import Databrary.Model.Measure
import Databrary.Model.Tag
import Databrary.Search.Service
import Databrary.Search.Document

solrDocId :: forall a . (Kinded a, Show (Id a)) => Id a -> BS.ByteString
solrDocId i = kindOf (undefined :: a) <> BSC.pack ('_' : show i)

solrParty :: Party -> Maybe Permission -> SolrDocument
solrParty Party{..} auth = SolrParty
  { solrId = solrDocId partyId
  , solrPartyId_i = partyId
  , solrPartyName_s = partySortName
  , solrPartyPreName_s = partyPreName
  , solrPartyAffiliation_s = partyAffiliation
  , solrPartyIsInstitution_b = isNothing partyAccount
  , solrPartyAuthorization_i = auth
  }

solrVolume :: Volume -> Maybe Citation -> SolrDocument
solrVolume Volume{..} cite = SolrVolume
  { solrId = solrDocId volumeId
  , solrVolumeId_i = volumeId
  , solrName_t = Just volumeName
  , solrBody_t = volumeBody
  , solrVolumeOwnerIds_is = ownerIds
  , solrVolumeOwnerNames_ss = ownerNames
  , solrCitation_t = citationHead <$> cite
  , solrCitationUrl_s = citationURL =<< cite
  , solrCitationYear_i = citationYear =<< cite
  } where
  (ownerIds, ownerNames) = unzip volumeOwners

solrContainer :: Container -> SolrDocument
solrContainer Container{..} = SolrContainer
  { solrId = solrDocId containerId
  , solrContainerId_i = containerId
  , solrVolumeId_i = volumeId containerVolume
  , solrName_t = containerName
  , solrContainerTop_b = containerTop
  , solrRelease_i = containerRelease
  }

solrAsset :: Asset -> SlotId -> SolrDocument
solrAsset Asset{..} SlotId{..} = SolrAsset
  { solrId = solrDocId assetId
  , solrAssetId_i = assetId
  , solrVolumeId_i = volumeId assetVolume
  , solrContainerId_i = slotContainerId
  , solrSegment = SolrSegment slotSegmentId
  , solrSegmentDuration_td = segmentLength slotSegmentId
  , solrName_t = assetName
  , solrRelease_i = assetRelease
  , solrFormat_i = formatId assetFormat
  }

solrExcerpt :: Excerpt -> SolrDocument
solrExcerpt Excerpt{ excerptAsset = AssetSegment{ segmentAsset = AssetSlot{ slotAsset = Asset{..}, assetSlot = ~(Just Slot{ slotContainer = container }) }, assetSegment = seg }, ..} = SolrExcerpt
  { solrId = BSC.pack $ "excerpt_" <> show assetId
    <> maybe "" (('_':) . show) (lowerBound $ segmentRange seg)
  , solrAssetId_i = assetId
  , solrVolumeId_i = volumeId assetVolume
  , solrContainerId_i = containerId container
  , solrSegment = SolrSegment seg
  , solrSegmentDuration_td = segmentLength seg
  , solrRelease_i = assetRelease
  }

solrRecord :: RecordSlot -> SolrDocument
solrRecord rs@RecordSlot{ slotRecord = r@Record{..}, recordSlot = Slot{..} } = SolrRecord
  { solrId = solrDocId recordId
    <> BSC.pack ('_' : show (containerId slotContainer))
  , solrRecordId_i = recordId
  , solrVolumeId_i = volumeId recordVolume
  , solrContainerId_i = containerId slotContainer
  , solrSegment = SolrSegment slotSegment
  , solrSegmentDuration_td = segmentLength slotSegment
  , solrRecordCategory_s = recordCategoryName recordCategory
  , solrRecordMeasures = SolrRecordMeasures $ map (\m -> (measureMetric m, measureDatum m)) $ getRecordMeasures r
  , solrRecordAge_ti = recordSlotAge rs
  }

solrTag :: Id Volume -> TagUseId -> SolrDocument
solrTag vi TagUseId{ useTagId = Tag{..}, tagSlotId = SlotId{..}, ..} = SolrTag
  { solrId = BSC.pack $ "tag_" <> show tagId
    <> ('_' : show slotContainerId)
    <> (if tagKeywordId then "" else '_' : show tagWhoId)
    <> maybe "" (('_':) . show) (lowerBound $ segmentRange slotSegmentId)
  , solrVolumeId_i = vi
  , solrContainerId_i = slotContainerId
  , solrSegment = SolrSegment slotSegmentId
  , solrSegmentDuration_td = segmentLength slotSegmentId
  , solrTagId_i = tagId
  , solrTag_s = tagName
  , solrKeyword_s = tagKeywordId ?> tagName
  , solrPartyId_i = tagWhoId
  }

newtype SolrContext = SolrContext { solrService :: Service }

instance Has Identity SolrContext where
  view _ = NotIdentified
instance Has SiteAuth SolrContext where
  view _ = view NotIdentified
instance Has Party SolrContext where
  view _ = view NotIdentified
instance Has (Id Party) SolrContext where
  view _ = view NotIdentified
instance Has Access SolrContext where
  view _ = view NotIdentified

makeHasRec ''SolrContext ['solrService]

newtype SolrM a = SolrM { runSolrM :: ReaderT SolrContext (InvertM BS.ByteString) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader SolrContext, MonadDB)

writeBlock :: BS.ByteString -> SolrM ()
writeBlock = SolrM . lift . give

writeDocuments :: [SolrDocument] -> SolrM ()
writeDocuments [] = return ()
writeDocuments d =
  writeBlock $ BSL.toStrict $ BSB.toLazyByteString $ Fold.foldMap (("},\"add\":{\"doc\":" <>) . JSON.encodeToBuilder . JSON.toJSON) d

writeUpdate :: SolrM () -> SolrM ()
writeUpdate f = do
  writeBlock "{\"delete\":{\"query\":\"*:*\""
  f
  writeBlock "}}"

joinContainers :: (a -> Slot -> b) -> [Container] -> [(a, SlotId)] -> [b]
joinContainers _ _ [] = []
joinContainers _ [] _ = error "joinContainers"
joinContainers f cl@(c:cr) al@((a, SlotId ci s):ar)
  | containerId c == ci = f a (Slot c s) : joinContainers f cl ar
  | otherwise = joinContainers f cr al

writeVolume :: (Volume, Maybe Citation) -> SolrM ()
writeVolume (v, vc) = do
  writeDocuments [solrVolume v vc]
  cl <- lookupVolumeContainers v
  writeDocuments $ map solrContainer cl
  writeDocuments . map (uncurry solrAsset) =<< lookupVolumeAssetSlotIds v
  -- this could be more efficient, but there usually aren't many:
  writeDocuments . map solrExcerpt =<< lookupVolumeExcerpts v
  writeDocuments . map solrRecord =<< joinContainers RecordSlot cl <$> lookupVolumeRecordSlotIds v
  writeDocuments . map (solrTag (volumeId v)) =<< lookupVolumeTagUseIds v

writeAllDocuments :: SolrM ()
writeAllDocuments = do
  mapM_ writeVolume =<< lookupVolumesCitations
  writeDocuments . map (uncurry solrParty) =<< lookupPartyAuthorizations

updateIndex :: Timestamp -> Service -> IO ()
updateIndex t rc = handle
  (\(e :: HC.HttpException) -> logMsg t ("solr update failed: " ++ show e) (serviceLogs rc))
  $ do
    _ <- HC.httpNoBody req
      { HC.path = HC.path req <> "update/json"
      , HC.queryString = "?commit=true"
      , HC.method = "POST"
      , HC.requestBody = HC.RequestBodyStreamChunked $ \wf -> do
        w <- runInvert $ runReaderT (runSolrM (writeUpdate writeAllDocuments)) (SolrContext rc)
        wf $ Fold.fold <$> w
      , HC.requestHeaders = (hContentType, "application/json") : HC.requestHeaders req
      } (serviceHTTPClient rc)
    t' <- getCurrentTime
    logMsg t' ("solr update complete " ++ show (diffUTCTime t' t)) (serviceLogs rc)
  where req = solrRequest (serviceSolr rc)
