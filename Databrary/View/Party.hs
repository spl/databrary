{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Party
  ( htmlPartyView
  , htmlPartyEdit
  , htmlPartySearch
  ) where

import Control.Monad (when, forM_)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Foldable as Fold
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>), mempty)
import Network.HTTP.Types (toQuery)
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
import Databrary.View.Paginate

import {-# SOURCE #-} Databrary.Controller.Angular
import {-# SOURCE #-} Databrary.Controller.Party
import {-# SOURCE #-} Databrary.Controller.Volume

htmlPartyView :: Party -> AuthRequest -> H.Html
htmlPartyView p req = htmlTemplate req (Just (partyName p)) $ \js -> do
  when (view p >= PermissionEDIT) $
    H.p $
      H.a H.! actionLink viewPartyEdit (TargetParty (partyId p)) js $ "edit"
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
  H.a H.! actionLink queryVolumes HTML (toQuery js <> [("party", Just $ BSC.pack $ show $ partyId p)]) $ "volumes"
  return ()

htmlPartyForm :: Maybe Party -> FormHtml TempFile
htmlPartyForm t = do
  field "prename" $ inputText $ partyPreName =<< t
  field "sortname" $ inputText $ partySortName <$> t
  field "affiliation" $ inputText $ partyAffiliation =<< t
  field "url" $ inputText $ show <$> (partyURL =<< t)

htmlPartyEdit :: Maybe Party -> AuthRequest -> FormHtml TempFile
htmlPartyEdit t = maybe
  (htmlForm "Create party" createParty HTML)
  (\p -> htmlForm
    ("Edit " <> partyName p)
    postParty (HTML, TargetParty (partyId p)))
  t
  (htmlPartyForm t)
  (const mempty)

htmlPartyList :: JSOpt -> [Party] -> H.Html
htmlPartyList js pl = H.ul $ forM_ pl $ \p -> H.li $ do
  H.h2
    $ H.a H.! actionLink viewParty (HTML, TargetParty (partyId p)) js
    $ H.text $ partyName p
  Fold.mapM_ H.text $ partyAffiliation p

htmlPartySearchForm :: PartyFilter -> FormHtml f
htmlPartySearchForm pf = do
  field "query" $ inputText $ partyFilterQuery pf
  field "authorization" $ inputEnum False $ partyFilterAuthorization pf
  field "institution" $ inputCheckbox $ fromMaybe False $ partyFilterInstitution pf

htmlPartySearch :: PartyFilter -> [Party] -> AuthRequest -> FormHtml f
htmlPartySearch pf pl req = htmlForm "Search users" queryParties HTML
  (htmlPartySearchForm pf)
  (\js -> htmlPaginate (htmlPartyList js) (partyFilterPaginate pf) pl (view req))
  req
