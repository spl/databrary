{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Databrary.Action.Route
  ( Method(..)
  , ActionRoute
  , actionURL
  , actionURI
  , actionMethod
  , action
  , multipartAction
  , API(..)
  , pathHTML
  , pathJSON
  , pathAPI
  ) where

import qualified Data.ByteString.Builder as BSB
import qualified Data.Invertible as I
import Network.HTTP.Types (Query)
import Network.URI (URI(..))
import qualified Web.Route.Invertible as R
import Web.Route.Invertible (Method(..))

import Databrary.HTTP.Request
import Databrary.HTTP.Route
import Databrary.HTTP.Path.Parser
import Databrary.Action.Run

type ActionRoute a = R.RouteAction a Action

actionURL :: Maybe Request -> R.RouteAction r a -> r -> Query -> BSB.Builder
actionURL req r a q
  | R.requestMethod rr == GET = routeURL req rr q
  | otherwise = error $ "actionURL: " ++ show rr
  where rr = R.requestActionRoute r a

actionURI :: Maybe Request -> R.RouteAction r a -> r -> Query -> URI
actionURI req r a q
  | R.requestMethod rr == GET = routeURI req rr q
  | otherwise = error $ "actionURI: " ++ show rr
  where rr = R.requestActionRoute r a

actionMethod :: R.RouteAction r a -> r -> Method
actionMethod r = R.requestMethod . R.requestActionRoute r

action :: Method -> PathParser r -> (r -> a) -> R.RouteAction r a
action m p a = R.routePath p R.>* R.routeMethod m `R.RouteAction` a

multipartAction :: R.RouteAction q a -> R.RouteAction q a
multipartAction (R.RouteAction r a) =
  R.RouteAction (r R.>* (R.routeAccept "multipart/form-data" R.>| R.unit)) a

data API
  = HTML
  | JSON
  deriving (Eq)

pathHTML :: PathParser ()
pathHTML = R.unit

pathJSON :: PathParser ()
pathJSON = "api"

pathAPI :: PathParser API
pathAPI = [I.biCase|Left () <-> JSON ; Right () <-> HTML|] R.>$< (pathJSON R.>|< pathHTML)
