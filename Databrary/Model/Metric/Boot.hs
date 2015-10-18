{-# LANGUAGE TemplateHaskell, DataKinds #-}
module Databrary.Model.Metric.Boot
  ( loadMetrics
  ) where

import Database.PostgreSQL.Typed.Array ()
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import Databrary.Service.DB
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.Metric.SQL

loadMetrics :: TH.ExpQ
loadMetrics = do
  l <- runTDB $ dbQuery $(selectQuery metricRow "ORDER BY id")
  TH.lift l
