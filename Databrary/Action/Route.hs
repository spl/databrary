{-# LANGUAGE OverloadedStrings #-}
module Databrary.Action.Route
  ( StdMethod(GET, POST)
  , actionURL
  , action
  , multipartAction
  , API(..)
  , pathHTML
  , pathJSON
  , pathAPI
  ) where

import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import Data.Monoid ((<>))
import Network.HTTP.Types (methodGet, StdMethod(..), renderStdMethod, Query, renderQueryBuilder)

import qualified Databrary.Iso as I
import Databrary.HTTP.Request
import Databrary.HTTP.Path.Parser
import Databrary.HTTP.Route

actionURL :: Maybe Request -> Route r a -> a -> Query -> BSB.Builder
actionURL req r@Route{ routeMethod = g } a q
  | g == methodGet = routeURL req r a <> renderQueryBuilder True q
  | otherwise = error $ "actionURL: " ++ BSC.unpack g

action :: StdMethod -> PathParser a -> (a -> r) -> Route r a
action m = Route (renderStdMethod m) False

multipartAction :: Route q a -> Route q a
multipartAction r = r{ routeMultipart = True }

data API
  = HTML
  | JSON
  deriving (Eq)

pathHTML :: PathParser ()
pathHTML = PathEmpty

pathJSON :: PathParser ()
pathJSON = "api"

pathAPI :: PathParser API
pathAPI = HTML =/= I.constant JSON I.<$> pathJSON
