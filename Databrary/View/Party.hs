{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Party
  ( htmlPartyView
  , htmlPartyForm
  , htmlPartySearchForm
  ) where

import qualified Text.Blaze.Html5 as H
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))

import Databrary.Ops
import Databrary.Action.Auth
import Databrary.Action
import Databrary.Model.Party
import Databrary.Store.Temp
import Databrary.Controller.Paths
import Databrary.View.Template
import Databrary.View.Form

import {-# SOURCE #-} Databrary.Controller.Party

htmlPartyView :: Party -> AuthRequest -> H.Html
htmlPartyView p req = htmlTemplate req (Just (partyName p)) $ do
  return ()

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
  field "access" $ inputEnum $ partyFilterAccess pf
  field "institution" $ inputCheckbox $ fromMaybe False $ partyFilterInstitution pf
