{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Search
  ( htmlUpdateIndex
  ) where

import Data.Monoid (mempty)

import Databrary.Action.Types
import Databrary.View.Form

import {-# SOURCE #-} Databrary.Controller.Search

htmlUpdateIndex :: Context -> FormHtml f
htmlUpdateIndex = htmlForm
  "update index"
  postUpdateIndex ()
  (return ())
  (const mempty)
