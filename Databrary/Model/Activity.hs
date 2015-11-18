{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}
module Databrary.Model.Activity
  ( lookupPartyActivity
  , lookupVolumeActivity
  , activityJSON
  ) where

import qualified Data.ByteString.Char8 as BSC
import Data.Function (on)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import qualified Data.Text as T

import Databrary.Ops
import Databrary.Has
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Identity.Types
import Databrary.Model.Id.Types
import Databrary.Model.Audit.Types
import Databrary.Model.Volume
import Databrary.Model.VolumeAccess
import Databrary.Model.Party
import Databrary.Model.Authorize
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
    <$> dbQuery $(selectQuery selectActivityParty $ "!WHERE party.id = ${partyId $ partyRow p} AND " ++ activityQual)
  ca <- chainPrev (const ()) . maskPasswords
    <$> dbQuery $(selectQuery selectActivityAccount $ "!WHERE account.id = ${partyId $ partyRow p}")
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
    volumeRowJSON v
      JSON..+? (("alias" JSON..=) <$> volumeAlias v))
activityTargetJSON (ActivityAccess a) =
  ("access", ["party" JSON..= partyJSON (volumeAccessParty a)],
    volumeAccessJSON a)

activityJSON :: Activity -> JSON.Object
activityJSON Activity{..} = JSON.object $ catMaybes
  [ Just $ "time" JSON..= auditWhen activityAudit
  , Just $ "action" JSON..= show (auditAction activityAudit)
  , Just $ "ip" JSON..= show (auditIp $ auditIdentity activityAudit)
  , Just $ "user" JSON..= partyRowJSON activityUser
  , Just $ typ JSON..= (new JSON..++ key)
  , HM.null old ?!> "old" JSON..= old
  ] where
  (new, old)
    | auditAction activityAudit == AuditActionRemove
      = (HM.empty, targ)
    | Just p <- activityPrev
    , (_, _, prev) <- activityTargetJSON p
    , int <- HM.filter id $ HM.intersectionWith (==) targ prev
      = (HM.difference targ int, HM.difference prev int)
    | otherwise
      = (targ, HM.empty)
  (typ, key, targ) = activityTargetJSON activityTarget
