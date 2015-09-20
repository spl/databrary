{-# LANGUAGE OverloadedStrings #-}
module Databrary.Action.Route
  ( StdMethod(GET, POST)
  , ActionRoute
  , actionURL
  , actionURI
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
import Network.HTTP.Types (methodGet, StdMethod(..), renderStdMethod, Query, renderQueryBuilder, renderQuery)
import Network.URI (URI(..))

import qualified Databrary.Iso as I
import Databrary.HTTP.Request
import Databrary.HTTP.Path.Parser
import Databrary.HTTP.Route
import Databrary.Action.Types

type ActionRoute a = Route Action a

actionURL :: Maybe Request -> Route r a -> a -> Query -> BSB.Builder
actionURL req r@Route{ routeMethod = g } a q
  | g == methodGet = routeURL req r a <> renderQueryBuilder True q
  | otherwise = error $ "actionURL: " ++ BSC.unpack g

actionURI :: Maybe Request -> Route r a -> a -> Query -> URI
actionURI req r@Route{ routeMethod = g } a q
  | g == methodGet = (routeURI req r a){ uriQuery = BSC.unpack $ renderQuery True q }
  | otherwise = error $ "actionURI: " ++ BSC.unpack g

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
