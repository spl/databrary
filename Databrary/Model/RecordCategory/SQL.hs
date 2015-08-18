{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.RecordCategory.SQL
  ( recordCategoryRow
  , recordTemplateRow
  ) where

import Databrary.Model.RecordCategory.Types
import Databrary.Model.SQL.Select

recordCategoryRow :: Selector -- RecordCategory
recordCategoryRow = selectColumns 'RecordCategory "record_category" ["id", "name", "description"]

recordTemplateRow :: Selector -- (Id Metric, Bool)
recordTemplateRow = selectColumns '(,) "record_template" ["metric", "ident"]
