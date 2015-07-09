{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Party
  ( htmlPartyView
  , htmlPartyForm
  , htmlPartySearchForm
  ) where

import Control.Monad (when)
import qualified Data.Foldable as Fold
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

import Databrary.Ops
import Databrary.Has (view)
import Databrary.Action.Auth
import Databrary.Action
import Databrary.Model.Permission
import Databrary.Model.Party
import Databrary.Model.ORCID
import Databrary.Store.Temp
import Databrary.Controller.Paths
import Databrary.View.Html
import Databrary.View.Template
import Databrary.View.Form

import {-# SOURCE #-} Databrary.Controller.Party

htmlPartyView :: Party -> AuthRequest -> H.Html
htmlPartyView p req = htmlTemplate req (Just (partyName p)) $ \js -> do
  when (view p >= PermissionEDIT) $
    H.p $
      H.a H.! actionLink viewPartyEdit (TargetParty (partyId p)) js [] $ "edit"
  H.img
    H.! HA.src (builderValue $ actionURL Nothing viewAvatar (partyId p) [])
  H.dl $ do
    Fold.forM_ (partyAffiliation p) $ \a -> do
      H.dt "affiliation"
      H.dd $ H.text a
    Fold.forM_ (partyURL p) $ \u -> do
      let us = show u
      H.dt "url"
      H.dd $ H.a H.! HA.href (H.stringValue us) $ H.string us
    Fold.forM_ (partyEmail p) $ \e -> do
      H.dt "email"
      H.dd $ H.a H.! HA.href (H.textValue $ "mailto:" <> e) $ H.text e
    Fold.forM_ (partyORCID p) $ \o -> do
      H.dt "orcid"
      H.dd $ H.a H.! HA.href (H.stringValue $ show $ orcidURL o) $ H.string $ show o

htmlPartyForm :: Maybe Party -> AuthRequest -> FormHtml TempFile
htmlPartyForm t req = maybe
  (htmlForm "Create party" createParty HTML)
  (\p -> htmlForm
    ("Edit " <> partyName p)
    postParty (HTML, TargetParty (partyId p)))
  t req $ do
  csrfForm req
  field "prename" $ inputText $ partyPreName =<< t
  field "sortname" $ inputText $ partySortName <$> t
  field "affiliation" $ inputText $ partyAffiliation =<< t
  field "url" $ inputText $ show <$> (partyURL =<< t)

htmlPartySearchForm :: PartyFilter -> AuthRequest -> FormHtml f
htmlPartySearchForm pf req = htmlForm "Search users" queryParties HTML req $ do
  field "query" $ inputText $ partyFilterQuery pf
  field "authorization" $ inputEnum $ partyFilterAuthorization pf
  field "institution" $ inputCheckbox $ fromMaybe False $ partyFilterInstitution pf
