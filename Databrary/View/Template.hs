{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Template
  ( htmlHeader
  , htmlFooter
  , htmlTemplate
  ) where

import Control.Monad (void, when)
import qualified Data.Foldable as Fold
import qualified Data.ByteString.Builder as BSB
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Version (showVersion)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Network.HTTP.Types (methodGet)
import qualified Network.Wai as Wai

import Paths_databrary (version)
import Databrary.Ops
import Databrary.Has (view)
import Databrary.Model.Identity
import Databrary.Action
import Databrary.Action.Auth
import Databrary.Controller.Paths
import Databrary.View.Html

import {-# SOURCE #-} Databrary.Controller.Angular
import {-# SOURCE #-} Databrary.Controller.Root
import {-# SOURCE #-} Databrary.Controller.Login
import {-# SOURCE #-} Databrary.Controller.Party
import Databrary.Controller.Web

htmlHeader :: Maybe BSB.Builder -> Maybe Bool -> H.Html
htmlHeader canon hasjs = do
  Fold.forM_ canon $ \c ->
    H.link
      H.! HA.rel "canonical"
      H.! HA.href (builderValue c)
  H.link
    H.! HA.rel "shortcut icon"
    H.! HA.href (builderValue $ actionURL Nothing webFile (Just $ staticPath ["icons", "favicon.png"]) [])
  H.link
    H.! HA.rel "start"
    H.! actionLink viewRoot HTML hasjs []
  Fold.forM_ ["news", "about", "access", "community"] $ \l -> H.link
    H.! HA.rel l
    H.! HA.href ("//databrary.org/" <> l <> ".html")

htmlFooter :: H.Html
htmlFooter = H.footer H.! HA.id "site-footer" H.! HA.class_ "site-footer" $
  H.div H.! HA.class_ "wrap" $
    H.div H.! HA.class_ "row" $ do
      H.div H.! HA.class_ "site-footer-links col-desktop-8 col-tablet-5 col-mobile-6" $
        H.p $ do
          void "Stay informed with "
          H.a H.! HA.href "http://databrary.org/contact/newsletter.html" $
            "our newsletter"
          "!"
      H.div H.! HA.class_ "site-footer-social col-desktop-7 col-tablet-4 col-mobile-6" $
        H.p $ do
          let sm n l a =
                H.a H.! HA.href l H.! HA.target "_blank" H.! HA.class_ "img" $
                  H.img H.! HA.id n H.! HA.src ("/web/images/social/16px/" <> n <> ".png") H.! HA.alt a
          void "Find us on "
          sm "twitter" "https://twitter.com/databrary" "Twitter"
          void ", "
          sm "facebook" "https://www.facebook.com/databrary" "Facebook"
          void ", "
          sm "linkedin" "https://www.linkedin.com/company/databrary-project" "LinkedIn"
          void ", "
          sm "google-plus" "https://plus.google.com/u/1/111083162045777800330/posts" "Google+"
          void ", and "
          sm "github" "https://github.com/databrary/" "GitHub"
          "."
      H.div H.! HA.class_ "site-footer-legal col" $ do
        H.p $
          "This service is based on work supported by the National Science Foundation under Grant No. BCS-1238599 and the Eunice Kennedy Shriver National Institute of Child Health and Human Development under Cooperative Agreement U01-HD-076595. Any opinions, findings, and conclusions or recommendations expressed in the material contributed here are those of the author(s) and do not necessarily reflect the views of the National Science Foundation or the Eunice Kennedy Shriver National Institute of Child Health and Human Development."
        H.p $ do
          void "Each dataset on Databrary represents an individual work owned by the party who contributed it. Data on Databrary is provided for non-commercial use and is subject to the terms of use outlined in the "
          H.a H.! HA.href "http://databrary.org/access/policies/agreement.html" H.! HA.target "_blank" $
            "Databrary Access Agreement"
          "."
        H.p $ do
          H.span H.! H.customAttribute "xmlns:dct" "http://purl.org/dc/terms/" H.! HA.href "http://purl.org/dc/dcmitype/Text" H.! H.customAttribute "property" "dct:title" H.! HA.rel "dct:type" $
            "Databrary.org documentation"
          void " is licensed under a "
          H.a H.! HA.rel "license" H.! HA.href "http://creativecommons.org/licenses/by-nc-sa/3.0/deed.en_US" $
            "Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License"
          "."
        H.p $ do
          void "["
          H.string $ showVersion version
          "]"
        H.ul H.! HA.class_ "site-footer-grants" $ do
          H.li $
            H.a H.! HA.href "http://www.nsf.gov/awardsearch/showAward?AWD_ID=1238599&HistoricalAwards=false" $ do
              H.img H.! HA.src "/web/images/grants/nsf.png" H.! HA.class_ "nsf"
              " BCS-1238599"
          H.li $
            H.a H.! HA.href "http://projectreporter.nih.gov/project_info_description.cfm?aid=8531595&icde=15908155&ddparam=&ddvalue=&ddsub=&cr=1&csb=default&cs=ASC" $ do
              H.img H.! HA.src "/web/images/grants/nih.png" H.! HA.class_ "nih"
              " U01-HD-076595"

htmlTemplate :: AuthRequest -> Maybe T.Text -> (Maybe Bool -> H.Html) -> H.Html
htmlTemplate req title body = H.docTypeHtml $ do
  H.head $ do
    htmlHeader canon hasjs
    H.title $ do
      Fold.mapM_ (\t -> H.toHtml t >> " || ") title
      "Databrary"
  H.body $ do
    when (hasjs /= Just True) $ Fold.forM_ canon $ \c -> H.div $ do
      H.preEscapedString "Our site works best with modern browsers (Firefox, Chrome, Safari &ge;6, IE &ge;10, and others). \
        \You are viewing the simple version of our site: some functionality may not be available. \
        \Try switching to the "
      H.a H.! HA.href (builderValue c) $ "modern version"
      " to see if it will work on your browser."
    H.section
      H.! HA.id "toolbar"
      H.! HA.class_ "toolbar"
      $ do
        H.h1 $ H.a
          H.! actionLink viewRoot HTML hasjs []
          $ "Databrary"
        foldIdentity
          (H.a H.! actionLink viewLogin () hasjs [] $ "login")
          (\_ -> do
            H.a H.! actionLink viewParty (HTML, TargetProfile) hasjs [] $ "profile"
            actionForm postLogout HTML $
              H.button
                H.! HA.type_ "submit"
                $ "logout")
          $ authIdentity req
    Fold.mapM_ (H.h1 . H.toHtml) title
    r <- body hasjs
    htmlFooter
    return r
  where
  (hasjs, nojs) = jsURL Nothing (view req)
  canon = Wai.requestMethod (view req) == methodGet && isJust hasjs ?> nojs
