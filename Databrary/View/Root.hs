{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Root
  ( htmlRoot
  ) where

import qualified Text.Blaze.Html5 as H

import Databrary.Has (view)
import Databrary.Model.Identity
import Databrary.Action.Auth
import Databrary.Action.Route
import Databrary.Controller.Paths
import Databrary.Controller.Login
import Databrary.Controller.Volume
import Databrary.Controller.Party
import Databrary.View.Template
import Databrary.View.Html

htmlRoot :: AuthRequest -> H.Html
htmlRoot req = htmlTemplate req Nothing $ \js -> do
  H.ul $ do
    H.li $ foldIdentity
      (H.a H.! actionLink viewLogin () js $ "login")
      (\_ -> H.a H.! actionLink viewParty (HTML, TargetProfile) js $ "profile")
      (view req)
    H.li $ H.a H.! actionLink queryVolumes HTML js $ "volumes"
    H.li $ H.a H.! actionLink queryParties HTML js $ "parties"
  return ()
