{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Pivot
  ( htmlPivot
  ) where

import Control.Monad (forM_)
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid ((<>))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

import qualified Databrary.JSON as JSON
import Databrary.Model.Volume.Types
import Databrary.Action
import Databrary.Controller.Angular
import Databrary.Controller.Web
import Databrary.Controller.CSV
import Databrary.View.Html
import Databrary.View.Template

htmlPivot :: Volume -> H.Html
htmlPivot vol = H.docTypeHtml $ do
  H.head $ do
    htmlHeader Nothing JSDefault
    forM_ ["jquery", "jquery-ui", "jquery.csv", "pivot"] $ \j ->
      H.script H.! HA.src (actionValue webFile (Just $ StaticPath $ "lib/" <> j <> ".min.js") ([] :: Query)) $ return ()
    H.script $ H.unsafeLazyByteString $ BSB.toLazyByteString $
      "$.get(" <> JSON.quoteByteString '"' (BSL.toStrict $ BSB.toLazyByteString $ actionURL Nothing csvVolume (volumeId vol) []) <> ", function(csv) {\
        \$('#pivot').pivotUI($.csv.toArrays(csv));\
      \});"
    H.link
      H.! HA.rel "stylesheet"
      H.! (actionLink webFile (Just $ StaticPath "lib/pivot.css") ([] :: Query))
  H.body $ do
    H.div H.! HA.id "pivot" $ return ()
    htmlFooter
