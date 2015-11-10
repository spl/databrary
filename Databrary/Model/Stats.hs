{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, RecordWildCards, TupleSections #-}
module Databrary.Model.Stats
  ( lookupSiteStats
  , siteStatsJSON
  ) where

import Control.Applicative ((<$>))
import Control.Monad (liftM2)
import qualified Data.Array.Unboxed as A
import Data.Int (Int64)
import Data.Maybe (mapMaybe)
import Database.PostgreSQL.Typed.Query (pgSQL)

import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.Permission.Types

useTDB

data SiteStats = SiteStats
  { statsAuthorizedSite :: A.Array Permission Int64
  }

lookupSiteStats :: MonadDB c m => m SiteStats
lookupSiteStats = do
  ac <- mapMaybe (uncurry $ liftM2 (,)) <$> dbQuery [pgSQL|SELECT site, count(child) FROM authorize_view WHERE parent = 0 AND child > 4 GROUP BY site|]
  return SiteStats
    { statsAuthorizedSite = A.accumArray (+) 0 (minBound, maxBound) ac
    }

siteStatsJSON :: SiteStats -> JSON.Object
siteStatsJSON SiteStats{..} = JSON.object
  [ "authorized" JSON..= A.elems statsAuthorizedSite
  ]
