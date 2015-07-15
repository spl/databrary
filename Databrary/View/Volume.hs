{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Volume
  ( htmlVolumeView
  , htmlVolumeForm
  , htmlVolumeLinksForm
  , htmlVolumeSearchForm
  ) where

import Control.Monad (when, forM_)
import qualified Data.Foldable as Fold
import Data.Monoid ((<>))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

import Databrary.Ops
import Databrary.Has (view)
import Databrary.Action.Auth
import Databrary.Action
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Model.Citation
import Databrary.HTTP.Form.View
import Databrary.Controller.Paths
import Databrary.View.Html
import Databrary.View.Template
import Databrary.View.Form

import {-# SOURCE #-} Databrary.Controller.Party
import {-# SOURCE #-} Databrary.Controller.Volume

htmlVolumeView :: Volume -> AuthRequest -> H.Html
htmlVolumeView v req = htmlTemplate req (Just (volumeName v)) $ \js -> do
  when (view v >= PermissionEDIT) $
    H.p $
      H.a H.! actionLink viewVolumeEdit (volumeId v) js [] $ "edit"
  H.img
    H.! HA.src (builderValue $ actionURL Nothing thumbVolume (volumeId v) [])
  H.dl $ do
    Fold.forM_ (getVolumeAlias v) $ \a -> do
      H.dt "alias"
      H.dd $ H.text a
    forM_ (volumeOwners v) $ \(p, n) -> do
      H.dt "owner"
      H.dd $ H.a H.! actionLink viewParty (HTML, TargetParty p) js [] $ H.text n
    Fold.forM_ (volumeBody v) $ \b -> do
      H.dt "body"
      H.dd $ H.text b -- format
    Fold.forM_ (volumeDOI v) $ \d -> do
      H.dt "doi"
      H.dd $ byteStringHtml d

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
