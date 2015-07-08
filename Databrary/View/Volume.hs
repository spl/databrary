{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Volume
  ( htmlVolumeForm
  , htmlVolumeLinksForm
  , htmlVolumeSearchForm
  ) where

import Data.Monoid ((<>))

import Databrary.Ops
import Databrary.Action.Auth
import Databrary.Action
import Databrary.Model.Volume
import Databrary.Model.Citation
import Databrary.HTTP.Form.View
import Databrary.View.Form

import {-# SOURCE #-} Databrary.Controller.Volume

htmlVolumeForm :: Maybe Volume -> Maybe Citation -> AuthRequest -> FormHtml f
htmlVolumeForm vol cite req = f req $ do
  csrfForm req
  field "name" $ inputText $ volumeName <$> vol
  field "alias" $ inputText $ volumeAlias =<< vol
  field "body" $ inputTextarea $ volumeBody =<< vol
  "citation" .:> do
    field "head" $ inputText $ citationHead <$> cite
    field "url" $ inputText $ fmap show $ citationURL =<< cite
    field "year" $ inputText $ fmap show $ citationYear =<< cite
  where
  f = maybe
    (htmlForm "Create volume" createVolume HTML)
    (\v -> htmlForm
      ("Edit " <> volumeName v)
      postVolume (HTML, volumeId v))
    vol

htmlVolumeLinksForm :: Volume -> [Citation] -> AuthRequest -> FormHtml f
htmlVolumeLinksForm vol links req = htmlForm "Edit volume links" postVolumeLinks (HTML, volumeId vol) req $ do
  csrfForm req
  withSubFormsViews links $ \link -> do
    field "head" $ inputText $ citationHead <$> link
    field "url" $ inputText $ fmap show $ citationURL =<< link

htmlVolumeSearchForm :: VolumeFilter -> AuthRequest -> FormHtml f
htmlVolumeSearchForm vf req = htmlForm "Search volumes" queryVolumes HTML req $ do
  csrfForm req
  field "query" $ inputText $ volumeFilterQuery vf
