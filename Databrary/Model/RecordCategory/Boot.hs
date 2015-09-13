{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.RecordCategory.Boot
  ( loadRecordCategories
  ) where

import Control.Applicative ((<$>))
import Database.PostgreSQL.Typed.Array ()
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import Databrary.Service.DB
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.RecordCategory.Types
import Databrary.Model.RecordCategory.SQL
import Databrary.Model.Id.Types
import Databrary.Model.Metric

loadRecordCategories :: TH.ExpQ -- [RecordCategory]
loadRecordCategories = runTDB
  (dbQuery (fmap ($ undefined) $(selectQuery recordCategoryRow "ORDER BY id"))
    >>= mapM (\c -> (,) c <$>
      dbQuery $(selectQuery recordTemplateRow "WHERE category = ${recordCategoryId c} ORDER BY ident DESC, metric")))
  >>= TH.listE . map (\(c, ts) ->
    TH.conE 'RecordCategory
      `TH.appE` TH.lift (recordCategoryId c)
      `TH.appE` TH.lift (recordCategoryName c)
      `TH.appE` TH.lift (recordCategoryDescription c)
      `TH.appE` TH.listE (map (\(m, i) -> TH.conE '(,)
        `TH.appE` (TH.varE 'getMetric' `TH.appE` TH.lift (m :: Id Metric))
        `TH.appE` TH.lift (i :: Bool)) ts))
