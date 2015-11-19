{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}
module Databrary.Model.Activity
  ( lookupPartyActivity
  , lookupVolumeActivity
  , lookupContainerActivity
  , activityJSON
  ) where

import Control.Arrow ((&&&))
import Control.Monad (forM)
import qualified Data.ByteString.Char8 as BSC
import Data.Function (on)
import qualified Data.HashMap.Strict as HM
import Data.List (foldl')
import qualified Data.Map as Map
import Data.Maybe (maybeToList, catMaybes)
import Data.Monoid ((<>))
import qualified Data.Text as T

import Databrary.Ops
import Databrary.Has
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Identity
import Databrary.Model.Id
import Databrary.Model.Audit
import Databrary.Model.Volume
import Databrary.Model.VolumeAccess
import Databrary.Model.Party
import Databrary.Model.Authorize
import Databrary.Model.Container
import Databrary.Model.Segment
import Databrary.Model.Slot
import Databrary.Model.Asset
import Databrary.Model.AssetSlot
import Databrary.Model.AssetRevision
import Databrary.Model.Activity.Types
import Databrary.Model.Activity.SQL

orderActivity :: Activity -> Activity -> Ordering
orderActivity = compare `on` auditWhen . activityAudit

mergeActivity :: [Activity] -> [Activity] -> [Activity]
mergeActivity = mergeBy $ \x y -> orderActivity x y <> LT

mergeActivities :: [[Activity]] -> [Activity]
mergeActivities = foldr1 mergeActivity

chainPrev :: Ord a => (ActivityTarget -> a) -> [Activity] -> [Activity]
chainPrev f = scan Map.empty where
  scan m (a@Activity{ activityAudit = Audit{ auditAction = act }, activityTarget = t }:l) = a{ activityPrev = p } : scan m' l where
    (p, m') = case act of
      AuditActionAdd -> (Nothing, Map.insert (f t) t m)
      AuditActionRemove -> Map.updateLookupWithKey (\_ _ -> Nothing) (f t) m
      AuditActionChange -> Map.insertLookupWithKey (const const) (f t) t m
      _ -> (activityPrev a, m)
  scan _ [] = []

maskPasswords :: [Activity] -> [Activity]
maskPasswords = mp HM.empty (0 :: Integer) where
  -- this could be done much more simply since passwords are never going to repeat in practice
  mp m c (a@Activity{ activityTarget = at@ActivityAccount{ activityAccountPassword = Just p } }:l)
    | Just i <- HM.lookup p m = f i : mp m c l
    | otherwise = f c : mp (HM.insert p c m) (succ c) l
    where f i = a{ activityTarget = at{ activityAccountPassword = Just $ BSC.pack $ show i } }
  mp m c (a:l) = a : mp m c l
  mp _ _ [] = []

lookupPartyActivity :: (MonadDB c m, MonadHasIdentity c m) => Party -> m [Activity]
lookupPartyActivity p = do
  ident <- peek
  pa <- chainPrev (const ())
    <$> dbQuery $(selectQuery selectActivityParty $ "WHERE party.id = ${partyId $ partyRow p} AND " ++ activityQual)
  ca <- chainPrev (const ()) . maskPasswords
    <$> dbQuery $(selectQuery selectActivityAccount $ "WHERE account.id = ${partyId $ partyRow p} ORDER BY audit_time") -- unqual: include logins
  aa <- chainPrev (partyId . partyRow . authorizeChild . authorization . activityAuthorize)
    <$> dbQuery $(selectQuery (selectActivityAuthorize 'p 'ident) $ "WHERE " ++ activityQual)
  return $ mergeActivities [pa, ca, aa]

lookupVolumeActivity :: (MonadDB c m, MonadHasIdentity c m) => Volume -> m [Activity]
lookupVolumeActivity vol = do
  ident <- peek
  va <- chainPrev (const ())
    <$> dbQuery $(selectQuery selectActivityVolume $ "!WHERE volume.id = ${volumeId $ volumeRow vol} AND " ++ activityQual)
  aa <- chainPrev (partyId . partyRow . volumeAccessParty . activityAccess)
    <$> dbQuery $(selectQuery (selectActivityAccess 'vol 'ident) $ "WHERE " ++ activityQual)
  return $ mergeActivities [va, aa]

lookupContainerActivity :: (MonadDB c m, MonadHasIdentity c m) => Container -> m [Activity]
lookupContainerActivity cont = do
  ca <- chainPrev (const ())
    <$> dbQuery $(selectQuery selectActivityContainer $ "WHERE container.id = ${containerId $ containerRow cont} AND " ++ activityQual)
  ra <- chainPrev (slotSegmentId . activitySlotId)
    <$> dbQuery $(selectQuery selectActivityRelease $ "WHERE slot_release.container = ${containerId $ containerRow cont} AND " ++ activityQual)

  let lar act@Activity{ activityPrev = Nothing, activityTarget = ActivityAssetSlot AssetSlotId{ slotAssetId = a } } = do
        ar <- lookupAssetRevision ba{ assetRow = (assetRow ba){ assetId = a } }
        return act{ activityPrev = ActivityAssetRevision <$> ar }
        where ba = blankAsset (containerVolume cont)
      lar a = return a
  asa <- mapM lar =<< chainPrev (slotAssetId . activityAssetSlot)
    <$> dbQuery $(selectQuery selectActivityAssetSlot $ "WHERE slot_asset.container = ${containerId $ containerRow cont} AND " ++ activityQual)

  caa <- chainPrev (assetId . activityAssetRow)
    <$> dbQuery $(selectQuery selectActivityAsset $ "JOIN slot_asset ON asset.id = slot_asset.asset WHERE slot_asset.container = ${containerId $ containerRow cont} AND " ++ activityQual)
  let uam m Activity{ activityAudit = Audit{ auditAction = AuditActionRemove, auditWhen = t }, activityTarget = ActivityAssetSlot AssetSlotId{ slotAssetId = a } } =
        Map.insert a t m
      uam m _ = m
      dam = flip $ Map.delete . assetId . activityAssetRow . activityTarget
      oal = Map.toList $ foldl' dam (foldl' uam Map.empty asa) caa
  oaa <- forM oal $ \(ai, at) ->
    chainPrev (const ())
      <$> dbQuery $(selectQuery selectActivityAsset $ "WHERE asset.id = ${ai} AND audit_time <= ${at} AND " ++ activityQual)

  cea <- chainPrev (activityAssetId &&& activitySegment)
    <$> dbQuery $(selectQuery selectActivityExcerpt $ "JOIN slot_asset ON excerpt.asset = slot_asset.asset WHERE slot_asset.container = ${containerId $ containerRow cont} AND " ++ activityQual)

  return $ mergeActivities (ca:ra:asa:cea:caa:oaa)

-- EDIT permission assumed for all
activityTargetJSON :: ActivityTarget -> (T.Text, [JSON.Pair], JSON.Object)
activityTargetJSON (ActivityParty p) =
  ("party", [],
    partyRowJSON p)
activityTargetJSON ActivityAccount{..} =
  ("account", [], JSON.object
    [ "email" JSON..= activityAccountEmail
    , "password" JSON..= activityAccountPassword
    ])
activityTargetJSON (ActivityAuthorize a) =
  ("authorize", ["party" JSON..= partyJSON (authorizeChild $ authorization a)],
    authorizeJSON a)
activityTargetJSON (ActivityVolume v) = 
  ("volume", [],
    volumeRowJSON v JSON..+?
      (("alias" JSON..=) <$> volumeAlias v))
activityTargetJSON (ActivityAccess a) =
  ("access", ["party" JSON..= partyJSON (volumeAccessParty a)],
    volumeAccessJSON a)
activityTargetJSON (ActivityContainer c) =
  ("container", [],
    containerRowJSON c JSON..+?
      (("date" JSON..=) <$> containerDate c))
activityTargetJSON ActivityRelease{..} =
  ("release", maybeToList $ segmentJSON $ slotSegmentId activitySlotId, JSON.object
    [ "release" JSON..= activityRelease
    ])
activityTargetJSON (ActivityAssetSlot (AssetSlotId a s)) =
  ("assets", ["id" JSON..= a], JSON.object $ maybeToList
    (segmentJSON . slotSegmentId . unId =<< s))
activityTargetJSON (ActivityAsset a) =
  ("asset", ["id" JSON..= assetId a],
    assetRowJSON a JSON..+?
      (("name" JSON..=) <$> assetName a))
activityTargetJSON (ActivityAssetRevision AssetRevision{..}) =
  (if revisionTranscode then "transcode" else "replace", ["id" JSON..= assetId (assetRow revisionAsset)],
    assetJSON revisionOrig)
activityTargetJSON ActivityExcerpt{..} =
  ("excerpt", ("id" JSON..= activityAssetId) : maybeToList (segmentJSON activitySegment), JSON.object $ maybeToList
    (("release" JSON..=) <$> activityExcerptRelease))

activityJSON :: Activity -> JSON.Object
activityJSON Activity{..} = JSON.object $ catMaybes
  [ Just $ "time" JSON..= auditWhen activityAudit
  , Just $ "action" JSON..= show (auditAction activityAudit)
  , Just $ "ip" JSON..= show (auditIp $ auditIdentity activityAudit)
  , Just $ "user" JSON..= auditWho (auditIdentity activityAudit)
  , Just $ typ JSON..= (new JSON..++ key)
  , HM.null old ?!> "old" JSON..= old
  ] where
  (new, old)
    | auditAction activityAudit == AuditActionRemove
      = (HM.empty, targ)
    | Just p@(ActivityAssetRevision AssetRevision{..}) <- activityPrev
    , (rtyp, _, orig) <- activityTargetJSON p
      = (targ JSON..+ (rtyp JSON..= True), orig)
    | Just p <- activityPrev
    , (_, _, prev) <- activityTargetJSON p
    , int <- HM.filter id $ HM.intersectionWith (==) targ prev
      = (HM.difference targ int, HM.difference prev int)
    | otherwise
      = (targ, HM.empty)
  (typ, key, targ) = activityTargetJSON activityTarget
