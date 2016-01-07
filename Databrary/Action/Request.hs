{-# LANGUAGE OverloadedStrings #-}
module Databrary.Action.Request
  ( isDatabraryClient
  ) where

import qualified Data.Foldable as Fold
import qualified Network.Wai as Wai

import Databrary.HTTP.Request

isDatabraryClient :: Wai.Request -> Bool
isDatabraryClient = Fold.any ("DatabraryClient" ==) . lookupRequestHeader "x-requested-with"
