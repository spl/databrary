{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Audit
  ( viewSiteAudit
  ) where

import Control.Arrow (second)
import Data.Function (on)
import Data.List (nubBy)
import Data.Ord (comparing)

import Databrary.Ops
import qualified Databrary.JSON as JSON
import Databrary.Model.Party
import Databrary.Model.Authorize
import Databrary.Model.Volume
import Databrary.Model.VolumeAccess
import Databrary.Model.Stats
import Databrary.HTTP.Path.Parser
import Databrary.Action

viewSiteAudit :: ActionRoute API
viewSiteAudit = action GET (pathAPI </< "audit") $ \api -> withAuth $ do
  ss <- lookupSiteStats -- TODO: cache
  vl <- map (second $ ("volume" JSON..=) . volumeJSON) . nubBy ((==) `on` volumeId . snd) <$> lookupVolumeActivity 8
  al <- map (second $ ("party"  JSON..=) . partyJSON)  . nubBy ((==) `on` partyId  . snd) <$> lookupAuthorizeActivity 8
  return $ case api of 
    JSON -> okResponse [] $ JSON.object
      [ "stats" JSON..= siteStatsJSON ss
      , "activity" JSON..= map ent (take 12 $ mergeBy ((fo .) . comparing fst) vl al)
      ]
  where
  ent (t, j) = JSON.object ["time" JSON..= t, j]
  fo GT = LT
  fo _ = GT
