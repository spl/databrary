module Databrary.View.Root
  ( htmlRoot
  ) where

import qualified Text.Blaze.Html5 as H

import Databrary.Action.Auth
import Databrary.View.Template

htmlRoot :: AuthRequest -> H.Html
htmlRoot req = htmlTemplate req Nothing $ \_ -> do
  return ()
