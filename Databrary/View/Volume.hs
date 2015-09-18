{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Databrary.View.Volume
  ( htmlVolumeView
  , htmlVolumeEdit
  , htmlVolumeLinksEdit
  , htmlVolumeSearch
  ) where

import Control.Monad (when, forM_)
import qualified Data.Foldable as Fold
import Data.Monoid ((<>), mempty)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

import Databrary.Ops
import Databrary.Has (view)
import Databrary.Action
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Model.Citation
import Databrary.HTTP.Form.View
import Databrary.Controller.Paths
import Databrary.View.Html
import Databrary.View.Template
import Databrary.View.Form
import Databrary.View.Paginate

import {-# SOURCE #-} Databrary.Controller.Angular
import {-# SOURCE #-} Databrary.Controller.Party
import {-# SOURCE #-} Databrary.Controller.Volume

htmlVolumeView :: Volume -> RequestContext -> H.Html
htmlVolumeView v req = htmlTemplate req (Just (volumeName v)) $ \js -> do
  when (view v >= PermissionEDIT) $
    H.p $
      H.a H.! actionLink viewVolumeEdit (volumeId v) js $ "edit"
  H.img
    H.! HA.src (builderValue $ actionURL Nothing thumbVolume (volumeId v) [])
  H.dl $ do
    Fold.forM_ (getVolumeAlias v) $ \a -> do
      H.dt "alias"
      H.dd $ H.text a
    forM_ (volumeOwners v) $ \(p, n) -> do
      H.dt "owner"
      H.dd $ H.a H.! actionLink viewParty (HTML, TargetParty p) js $ H.text n
    Fold.forM_ (volumeBody v) $ \b -> do
      H.dt "body"
      H.dd $ H.text b -- format
    Fold.forM_ (volumeDOI v) $ \d -> do
      H.dt "doi"
      H.dd $ byteStringHtml d

htmlVolumeForm :: Maybe Volume -> Maybe Citation -> FormHtml f
htmlVolumeForm vol cite = do
  field "name" $ inputText $ volumeName <$> vol
  field "alias" $ inputText $ volumeAlias =<< vol
  field "body" $ inputTextarea $ volumeBody =<< vol
  "citation" .:> do
    field "head" $ inputText $ citationHead <$> cite
    field "url" $ inputText $ fmap show $ citationURL =<< cite
    field "year" $ inputText $ fmap show $ citationYear =<< cite

htmlVolumeEdit :: Maybe (Volume, Maybe Citation) -> RequestContext -> FormHtml f
htmlVolumeEdit Nothing = htmlForm "Create volume" createVolume HTML (htmlVolumeForm Nothing Nothing) (const mempty)
htmlVolumeEdit (Just (v, cite)) = htmlForm ("Edit " <> volumeName v) postVolume (HTML, volumeId v) (htmlVolumeForm (Just v) cite) (const mempty)

htmlVolumeLinksEdit :: Volume -> [Citation] -> RequestContext -> FormHtml f
htmlVolumeLinksEdit vol links = htmlForm "Edit volume links" postVolumeLinks (HTML, volumeId vol)
  (withSubFormsViews links $ \link -> do
    field "head" $ inputText $ citationHead <$> link
    field "url" $ inputText $ fmap show $ citationURL =<< link)
  (const mempty)

htmlVolumeList :: JSOpt -> [Volume] -> H.Html
htmlVolumeList js vl = H.ul $ forM_ vl $ \v -> H.li $ do
  H.h2
    $ H.a H.! actionLink viewVolume (HTML, volumeId v) js
    $ H.text $ volumeName v
  H.ul $ forM_ (volumeOwners v) $ \(p, o) -> H.li $ do
    H.a H.! actionLink viewParty (HTML, TargetParty p) js
      $ H.text o
  Fold.mapM_ (H.p . H.text) $ volumeBody v

htmlVolumeSearch :: VolumeFilter -> [Volume] -> RequestContext -> FormHtml f
htmlVolumeSearch VolumeFilter{..} vl req = htmlForm "Volume search" queryVolumes HTML
  (field "query" $ inputText volumeFilterQuery)
  (\js -> htmlPaginate (htmlVolumeList js) volumeFilterPaginate vl (view req))
  req
