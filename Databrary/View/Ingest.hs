{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Ingest
  ( htmlIngestForm
  ) where

import qualified Data.Aeson as JSON
import Data.Monoid ((<>), mempty)

import Databrary.Action.Auth
import Databrary.Model.Volume
import Databrary.View.Form

import {-# SOURCE #-} Databrary.Controller.Ingest

htmlIngestForm :: Volume -> AuthRequest -> FormHtml JSON.Value
htmlIngestForm v = htmlForm
  ("Ingest " <> volumeName v)
  postIngest (volumeId v)
  (do
    field "run" $ inputCheckbox False
    field "overwrite" $ inputCheckbox False
    field "json" $ inputFile)
  (const mempty)
