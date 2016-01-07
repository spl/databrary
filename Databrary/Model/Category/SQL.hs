{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Category.SQL
  ( categoryRow
  ) where

import Databrary.Model.Category.Types
import Databrary.Model.SQL.Select

categoryRow :: Selector -- Category
categoryRow = selectColumns 'Category "category" ["id", "name", "description"]
