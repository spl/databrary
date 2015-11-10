{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Stats
  ( viewStats
  ) where

import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Model.Stats

viewStats :: ActionRoute API
viewStats = action GET (pathAPI </< "stats") $ \api -> withoutAuth $ do
  ss <- lookupSiteStats -- TODO: cache
  return $ case api of
    JSON -> okResponse [] (siteStatsJSON ss)
