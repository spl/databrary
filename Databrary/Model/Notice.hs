{-# LANGUAGE TemplateHaskell, DataKinds, OverloadedStrings #-}
module Databrary.Model.Notice
  ( Delivery(..)
  , Notice(..)
  ) where

import qualified Data.Aeson as JSON
import Data.Int (Int16)
import Database.PostgreSQL.Typed.Types (PGParameter(..), PGColumn(..))
import Database.PostgreSQL.Typed.Enum (PGEnum)

import Databrary.HTTP.Form.Deform
import Databrary.Model.Kind
import Databrary.Model.Enum
import Databrary.Model.Notice.Boot

makeDBEnum "notice_delivery" "Delivery"

makeNotice

instance PGParameter "smallint" Notice where
  pgEncode t = pgEncode t . (fromIntegral :: Int -> Int16) . fromEnum
  pgEncodeValue e t = pgEncodeValue e t . (fromIntegral :: Int -> Int16) . fromEnum
  pgLiteral t = pgLiteral t . (fromIntegral :: Int -> Int16) . fromEnum
instance PGColumn "smallint" Notice where
  pgDecode t = toEnum . (fromIntegral :: Int16 -> Int) . pgDecode t
  pgDecodeValue e t = toEnum . (fromIntegral :: Int16 -> Int) . pgDecodeValue e t

instance PGEnum Notice
instance Kinded Notice where
  kindOf _ = "notice"
instance DBEnum Notice
instance JSON.ToJSON Notice where
  toJSON = JSON.toJSON . fromEnum
instance JSON.FromJSON Notice where
  parseJSON = parseJSONEnum
instance Deform f Notice where
  deform = enumForm
