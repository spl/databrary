{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Activity
  ( lookupVolumeActivity
  ) where

import Data.Function (on)
import qualified Data.Map as Map
import Data.Monoid ((<>))

import Databrary.Ops
import Databrary.Has
import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Identity.Types
import Databrary.Model.Id.Types
import Databrary.Model.Audit.Types
import Databrary.Model.Volume.Types
import Databrary.Model.VolumeAccess.Types
import Databrary.Model.Party.Types
import Databrary.Model.Activity.Types
import Databrary.Model.Activity.SQL

orderActivity :: Activity -> Activity -> Ordering
orderActivity = compare `on` activityWhen

mergeActivity :: [Activity] -> [Activity] -> [Activity]
mergeActivity = mergeBy $ \x y -> orderActivity x y <> LT

mergeActivities :: [[Activity]] -> [Activity]
mergeActivities = foldr1 mergeActivity

chainPrev :: Ord a => (ActivityTarget -> a) -> [Activity] -> [Activity]
chainPrev f = scan Map.empty where
  scan m (a@Activity{ activityAction = act, activityTarget = t }:l) = a{ activityPrev = p } : scan m' l where
    (p, m') = case act of
      AuditActionAdd -> (Nothing, Map.insert (f t) t m)
      AuditActionRemove -> Map.updateLookupWithKey (\_ _ -> Nothing) (f t) m
      AuditActionChange -> Map.insertLookupWithKey (const const) (f t) t m
      _ -> (activityPrev a, m)
  scan _ [] = []

lookupVolumeActivity :: (MonadDB c m, MonadHasIdentity c m) => Volume -> m [Activity]
lookupVolumeActivity vol = do
  ident <- peek
  va <- chainPrev (const ())
    <$> dbQuery $(selectQuery selectActivityVolume $ "!WHERE volume.id = ${volumeId $ volumeRow vol} AND " ++ activityQual)
  aa <- chainPrev (partyId . partyRow . volumeAccessParty . activityAccess)
    <$> dbQuery $(selectQuery (selectActivityAccess 'vol 'ident) $ "WHERE " ++ activityQual)
  return $ mergeActivities [va, aa]
