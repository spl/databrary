{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Citation
  ( getCitation
  ) where

import Data.Aeson (toJSON)

import Databrary.Has (focusIO)
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Action.Types
import Databrary.Controller.Form
import Databrary.Model.Citation.CrossRef

getCitation :: ActionRoute ()
getCitation = action GET (pathJSON </< "cite") $ \() -> withoutAuth $ do
  url <- runForm Nothing $ "url" .:> deform
  cite <- maybeAction =<< focusIO (lookupCitation url)
  return $ okResponse [] $ toJSON cite
