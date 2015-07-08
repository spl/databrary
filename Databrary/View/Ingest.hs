{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Ingest
  ( htmlIngestForm
  ) where

import qualified Data.Aeson as JSON
import Data.Monoid ((<>))

import Databrary.Action.Auth
import Databrary.Model.Volume
import Databrary.View.Form

import {-# SOURCE #-} Databrary.Controller.Ingest

htmlIngestForm :: Volume -> AuthRequest -> FormHtml JSON.Value
htmlIngestForm v req = htmlForm
  ("Ingest " <> volumeName v)
  postIngest (volumeId v)
  req $ do
  csrfForm req
  field "run" $ inputCheckbox False
  field "overwrite" $ inputCheckbox False
  field "json" $ inputFile
