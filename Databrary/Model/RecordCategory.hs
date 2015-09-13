{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}
module Databrary.Model.RecordCategory
  ( module Databrary.Model.RecordCategory.Types
  , allRecordCategories
  , getRecordCategory
  , getRecordCategory'
  , recordCategoryJSON
  ) where

import Control.Applicative ((<$>))
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (catMaybes)

import qualified Databrary.JSON as JSON
import Databrary.Model.Id
import Databrary.Model.Metric
import Databrary.Model.RecordCategory.Types
import Databrary.Model.RecordCategory.Boot

allRecordCategories :: [RecordCategory]
allRecordCategories = $(loadRecordCategories)

recordCategoriesById :: IntMap.IntMap RecordCategory
recordCategoriesById = IntMap.fromAscList $ map (\a -> (fromIntegral $ unId $ recordCategoryId a, a)) allRecordCategories

getRecordCategory :: Id RecordCategory -> Maybe RecordCategory
getRecordCategory (Id i) = IntMap.lookup (fromIntegral i) recordCategoriesById

getRecordCategory' :: Id RecordCategory -> RecordCategory
getRecordCategory' (Id i) = recordCategoriesById IntMap.! fromIntegral i

recordCategoryJSON :: RecordCategory -> JSON.Object
recordCategoryJSON RecordCategory{..} = JSON.record recordCategoryId $ catMaybes
  [ Just $ "name" JSON..= recordCategoryName
  , Just $ "ident" JSON..= [ metricId m | (m, True) <- recordCategoryTemplate ]
  , Just $ "template" JSON..= map (metricId . fst) recordCategoryTemplate
  , ("description" JSON..=) <$> recordCategoryDescription
  ]
