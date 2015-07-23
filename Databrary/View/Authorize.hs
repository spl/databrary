{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Authorize
  ( htmlAuthorizeForm
  ) where

import Data.Monoid (mempty)
import qualified Data.Text as T

import Databrary.Action
import Databrary.View.Form
import Databrary.Model.Party
import Databrary.Model.Permission
import Databrary.Model.Authorize
import Databrary.Controller.Paths

import {-# SOURCE #-} Databrary.Controller.Authorize

htmlAuthorizeForm :: Authorize -> AuthRequest -> FormHtml f
htmlAuthorizeForm a = htmlForm
  ("Authorize " `T.append` partyName child)
  postAuthorize (HTML, TargetParty (partyId parent), AuthorizeTarget False (partyId child))
  (do
    field "site" $ inputEnum True $ Just $ accessSite a
    field "member" $ inputEnum True $ Just $ accessMember a
    field "expires" $ inputText $ Just $ show $ authorizeExpires a
    field "delete" $ inputCheckbox False)
  (const mempty)
  where
  Authorization
    { authorizeChild = child
    , authorizeParent = parent
    } = authorization a

