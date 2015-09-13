{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Format.Boot
  ( loadFormats
  ) where

import Database.PostgreSQL.Typed.Array ()
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import Databrary.Service.DB
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.Format.SQL

loadFormats :: TH.ExpQ
loadFormats = do
  l <- runTDB $ dbQuery $(selectQuery formatRow "ORDER BY id")
  TH.lift l
