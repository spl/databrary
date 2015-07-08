{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Comment
  ( htmlCommentForm
  ) where

import qualified Data.Text as T

import Databrary.Action.Auth
import Databrary.Action
import Databrary.Model.Slot
import Databrary.View.Form

import {-# SOURCE #-} Databrary.Controller.Comment

htmlCommentForm :: Slot -> AuthRequest -> FormHtml f
htmlCommentForm slot req = htmlForm "Comment" postComment (HTML, slotId slot) req $ do
  csrfForm req
  field "text" $ inputText (Nothing :: Maybe T.Text)
