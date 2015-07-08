{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Record
  ( htmlRecordForm
  , htmlRecordMeasureForm
  ) where

import qualified Data.ByteString.Char8 as BSC

import Databrary.Action.Auth
import Databrary.Action
import Databrary.Model.Volume
import Databrary.Model.Record
import Databrary.Model.RecordCategory
import Databrary.Model.Metric
import Databrary.View.Form

import {-# SOURCE #-} Databrary.Controller.Record

htmlRecordForm :: Volume -> AuthRequest -> FormHtml f
htmlRecordForm vol req = htmlForm "Create record"
  createRecord (HTML, volumeId vol) req $ do
  csrfForm req
  field "category" $ inputSelect Nothing $ ("", "<record>") : map (\c -> (BSC.pack $ show $ recordCategoryId c, recordCategoryName c)) allRecordCategories

htmlRecordMeasureForm :: Record -> Metric -> AuthRequest -> FormHtml f
htmlRecordMeasureForm rec met req = htmlForm "Set measure"
  postRecordMeasure (HTML, recordId rec, metricId met) req $ do
  csrfForm req
  field "datum" $ inputText (Nothing :: Maybe String)
