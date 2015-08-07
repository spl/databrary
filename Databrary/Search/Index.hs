{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module Databrary.Search.Index
  ( solrDocuments
  ) where

import Control.Monad.Trans.Reader (ReaderT(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (isNothing)
import Data.Monoid ((<>))

import Databrary.Ops
import Databrary.Has (Has(..), makeHasRec)
import Databrary.Service.Types
import Databrary.Model.Kind
import Databrary.Model.Id.Types
import Databrary.Model.Segment
import Databrary.Model.Permission.Types
import Databrary.Model.Identity.Types
import Databrary.Model.Party.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Citation
import Databrary.Model.Container
import Databrary.Model.Slot.Types
import Databrary.Model.Format.Types
import Databrary.Model.Asset.Types
import Databrary.Model.AssetSlot
import Databrary.Model.AssetSegment.Types
import Databrary.Model.Record.Types
import Databrary.Model.RecordSlot
import Databrary.Model.Measure
import Databrary.Model.Tag.Types
import Databrary.Search.Types

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

solrAsset :: AssetSlot -> SolrDocument
solrAsset AssetSlot{ slotAsset = Asset{..}, assetSlot = ~(Just Slot{..}) } = SolrAsset
  { solrId = solrDocId assetId
  , solrAssetId_i = assetId
  , solrVolumeId_i = volumeId assetVolume
  , solrContainerId_i = containerId slotContainer
  , solrSegment_s = slotSegment
  , solrSegmentDuration_td = segmentLength slotSegment
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
  , solrSegment_s = seg
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
  , solrSegment_s = slotSegment
  , solrSegmentDuration_td = segmentLength slotSegment
  , solrRecordMeasures = SolrRecordMeasures $ map (\m -> (measureMetric m, measureDatum m)) $ getRecordMeasures r
  , solrRecordAge_ti = recordSlotAge rs
  }

solrTag :: TagUse -> SolrDocument
solrTag TagUse{ useTag = Tag{..}, tagSlot = Slot{..}, ..} = SolrTag
  { solrId = BSC.pack $ "tag_" <> show tagId
    <> ('_' : show (containerId slotContainer))
    <> (if tagKeyword then "" else '_' : show (partyId (accountParty tagWho)))
    <> maybe "" (('_':) . show) (lowerBound $ segmentRange slotSegment)
  , solrVolumeId_i = volumeId (containerVolume slotContainer)
  , solrContainerId_i = containerId slotContainer
  , solrSegment_s = slotSegment
  , solrSegmentDuration_td = segmentLength slotSegment
  , solrTagId_i = tagId
  , solrTag_s = tagName
  , solrKeyword_s = tagKeyword ?> tagName
  , solrPartyId_i = partyId (accountParty tagWho)
  }

newtype SolrContext = SolrContext { contextService :: Service }

instance Has Identity SolrContext where
  view _ = UnIdentified
instance Has SiteAuth SolrContext where
  view _ = view UnIdentified
instance Has Party SolrContext where
  view _ = view UnIdentified
instance Has (Id Party) SolrContext where
  view _ = view UnIdentified
instance Has Access SolrContext where
  view _ = view UnIdentified

makeHasRec ''SolrContext ['contextService]

type SolrM a = ReaderT SolrContext IO a
-- newtype SolrM a = SolrM { runSolrM :: ReaderT Service IO a } deriving (Functor, Applicative, Monad, MonadIO, MonadBase IO, MonadReader Service)

joinContainers :: (a -> Slot -> b) -> [Container] -> [(a, SlotId)] -> [b]
joinContainers _ _ [] = []
joinContainers _ [] _ = error "joinContainers"
joinContainers f cl@(c:cr) al@((a, SlotId ci s):ar)
  | containerId c == ci = f a (Slot c s) : joinContainers f cl ar
  | otherwise = joinContainers f cr al

volumeDocuments :: (Volume, Maybe Citation) -> SolrM [SolrDocument]
volumeDocuments (v, vc) = do
  cl <- lookupVolumeContainers v
  al <- joinContainers ((. Just) . AssetSlot) cl <$> lookupVolumeAssetSlotIds v
  rl <- joinContainers RecordSlot cl <$> lookupVolumeRecordSlotIds v
  return
    $ solrVolume v vc
    : map solrContainer cl
    ++ map solrAsset al
    ++ map solrRecord rl

solrDocuments :: SolrM [SolrDocument]
solrDocuments =
  fmap concat . mapM volumeDocuments =<< lookupVolumesCitations
  -- parties
