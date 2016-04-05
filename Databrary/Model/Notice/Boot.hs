{-# LANGUAGE TemplateHaskell, QuasiQuotes, DataKinds #-}
module Databrary.Model.Notice.Boot
  ( makeNotice
  ) where

import Data.Int (Int16)
import Data.Ix (Ix)
import Data.Typeable (Typeable)
import Database.PostgreSQL.Typed.Query (pgSQL)
import qualified Language.Haskell.TH as TH

import Databrary.Service.DB

useTDB

makeNotice :: TH.DecsQ
makeNotice = do
  nl <- fill (0 :: Int16) <$> runTDB (dbQuery [pgSQL|SELECT id, name FROM notice WHERE id >= 0 ORDER BY id|])
  return
    [ TH.DataD [] (TH.mkName "Notice") [] (map (\n -> TH.NormalC (TH.mkName n) []) nl) [''Eq, ''Ord, ''Enum, ''Ix, ''Bounded, ''Show, ''Typeable]
    ]
  where
  fill i l@((d, n):r) = case compare i d of
    LT -> ("Notice_"++show i) : fill (succ i) l -- unused gap, hopefully rare and unused
    EQ -> ("Notice"++n) : fill (succ i) r
    GT -> error "makeNotice: out of order"
  fill _ [] = []
