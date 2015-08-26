module Databrary.View.Error where

import qualified Text.Blaze.Html5 as H

import Databrary.Action.Types

htmlNotFound :: Context -> H.Html
htmlForbidden :: Context -> H.Html
