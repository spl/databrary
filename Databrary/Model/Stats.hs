{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, RecordWildCards, TupleSections #-}
module Databrary.Model.Stats
  ( lookupSiteStats
  , siteStatsJSON
  ) where

import Control.Arrow ((***))
import Control.Monad (liftM2)
import qualified Data.Array.Unboxed as A
import Data.Int (Int64)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (toBoundedInteger)
import qualified Data.Text as T
import Database.PostgreSQL.Typed.Query (pgSQL)

import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.Permission.Types
import Databrary.Model.Offset
import Databrary.Model.Id.Types
import Databrary.Model.RecordCategory.Types

useTDB

data SiteStats = SiteStats
  { statsAuthorizedSite :: !(A.Array Permission Int64)
  , statsVolumes, statsVolumesShared :: !Int64
  , statsAssets :: !Int64
  , statsAssetDuration :: !Offset
  , statsAssetBytes :: !Int64
  , statsRecords :: [(Id RecordCategory, Int64)]
  }

lookupSiteStats :: MonadDB c m => m SiteStats
lookupSiteStats = do
  ac <- dbQuery [pgSQL|SELECT site, count(child) FROM authorize_view WHERE parent = 0 AND child > 4 GROUP BY site|]
  v <- dbQuery1' [pgSQL|SELECT count(id) FROM volume|]
  vs <- dbQuery1' [pgSQL|SELECT count(volume) FROM volume_access WHERE party = 0 AND children >= 'PUBLIC'|]
  (a, ad, ab) <- dbQuery1' [pgSQL|SELECT count(id), sum(duration), sum(size) FROM asset JOIN slot_asset ON asset = id|]
  rc <- dbQuery [pgSQL|SELECT category, count(id) FROM record GROUP BY category|]
  return SiteStats
    { statsAuthorizedSite = A.accumArray (+) 0 (minBound, maxBound) $ l ac
    , statsVolumes = z v
    , statsVolumesShared = z vs
    , statsAssets = z a
    , statsAssetDuration = z ad
    , statsAssetBytes = z $ toBoundedInteger =<< ab
    , statsRecords = l rc
    }
  where
  z :: Num a => Maybe a -> a
  z = fromMaybe 0
  l :: [(Maybe a, Maybe b)] -> [(a, b)]
  l = mapMaybe (uncurry $ liftM2 (,))

siteStatsJSON :: SiteStats -> JSON.Object
siteStatsJSON SiteStats{..} = JSON.object
  [ "authorized" JSON..= A.elems statsAuthorizedSite
  , "volumes" JSON..= statsVolumes
  , "shared" JSON..= statsVolumesShared
  , "assets" JSON..= statsAssets
  , "duration" JSON..= statsAssetDuration
  , "bytes" JSON..= statsAssetBytes
  , "records" JSON..= JSON.object (map (T.pack . show *** JSON.toJSON) statsRecords)
  ]
