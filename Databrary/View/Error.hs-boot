module Databrary.View.Error where

import qualified Text.Blaze.Html5 as H

import Databrary.Action.App

htmlNotFound :: AppRequest -> H.Html
htmlForbidden :: AppRequest -> H.Html
