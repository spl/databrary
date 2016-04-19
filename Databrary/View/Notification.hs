{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Databrary.View.Notification
  ( htmlNotification
  ) where

import qualified Text.Blaze.Html5 as H

import Databrary.Model.Notification

htmlNotification :: Notification -> H.Html
htmlNotification Notification{..} = do
  return ()
