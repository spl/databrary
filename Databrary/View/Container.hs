{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Container
  ( htmlContainerForm
  ) where

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))

import Databrary.Action.Auth
import Databrary.Action
import Databrary.Model.Volume
import Databrary.Model.Container
import Databrary.Model.Slot
import Databrary.View.Form

import {-# SOURCE #-} Databrary.Controller.Container

htmlContainerForm :: Either Volume Container -> AuthRequest -> FormHtml f
htmlContainerForm targ req = f req $ do
  csrfForm req
  field "name" $ inputText (containerName =<< cont)
  field "date" $ inputDate (containerDate =<< cont)
  field "release" $ inputEnum (containerRelease =<< cont)
  where
  cont = either (const Nothing) Just targ
  f = either
    (\v -> htmlForm "Create container" createContainer (HTML, volumeId v))
    (\c -> htmlForm ("Edit container " <> fromMaybe "" (containerName c)) postContainer (HTML, containerSlotId $ containerId c))
    targ
