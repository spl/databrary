{-# LANGUAGE TemplateHaskell, DataKinds #-}
module Databrary.Model.Category.Boot
  ( loadCategories
  ) where

import Database.PostgreSQL.Typed.Array ()
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import Databrary.Service.DB
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.Category.SQL

loadCategories :: TH.ExpQ -- [Category]
loadCategories = do
  l <- runTDB $ dbQuery $(selectQuery categoryRow "ORDER BY id")
  TH.lift l
