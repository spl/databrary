{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}
module Databrary.Model.Activity
  ( lookupVolumeActivity
  , activityJSON
  ) where

import qualified Data.Foldable as Fold
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

-- EDIT permission assumed for all
activityTargetJSON :: ActivityTarget -> (T.Text, [JSON.Pair], JSON.Object)
activityTargetJSON (ActivityParty p) =
  ("party", [],
    partyRowJSON p)
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
  [ Just $ "time" JSON..= activityWhen
  , Just $ "action" JSON..= show activityAction
  , Just $ "user" JSON..= partyRowJSON activityUser
  , Just $ t JSON..= new'
  , HM.null old' ?!> "old" JSON..= old'
  ] where
  (t, key, new) = activityTargetJSON activityTarget
  new' = HM.difference new int JSON..++ key
  old' = HM.difference old int
  (_, _, old) = Fold.foldMap activityTargetJSON activityPrev
  int = HM.filter id $ HM.intersectionWith (==) new old
