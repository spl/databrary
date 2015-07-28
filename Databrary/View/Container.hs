{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Container
  ( htmlContainerEdit
  ) where

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>), mempty)

import Databrary.Action.Auth
import Databrary.Action
import Databrary.Model.Volume
import Databrary.Model.Container
import Databrary.Model.Slot
import Databrary.View.Form

import {-# SOURCE #-} Databrary.Controller.Container

htmlContainerForm :: Maybe Container -> FormHtml f
htmlContainerForm cont = do
  field "name" $ inputText (containerName =<< cont)
  field "date" $ inputDate (containerDate =<< cont)
  field "release" $ inputEnum False (containerRelease =<< cont)

htmlContainerEdit :: Either Volume Container -> AuthRequest -> FormHtml f
htmlContainerEdit (Left v)  = htmlForm "Create container" createContainer (HTML, volumeId v) (htmlContainerForm Nothing) (const mempty)
htmlContainerEdit (Right c) = htmlForm ("Edit container " <> fromMaybe "" (containerName c)) postContainer (HTML, containerSlotId $ containerId c) (htmlContainerForm $ Just c) (const mempty)
