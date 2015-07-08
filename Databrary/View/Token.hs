{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Token
  ( htmlPasswordToken
  ) where

import Databrary.Model.Id
import Databrary.Model.Token
import Databrary.Action
import Databrary.View.Form

import {-# SOURCE #-} Databrary.Controller.Token

htmlPasswordToken :: Id LoginToken -> AuthRequest -> FormHtml f
htmlPasswordToken tok req = htmlForm "Reset Password"
  postPasswordToken (HTML, tok) req $ do
  field "once" inputPassword
  field "again" inputPassword
