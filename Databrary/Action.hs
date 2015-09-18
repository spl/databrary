{-# LANGUAGE OverloadedStrings #-}
module Databrary.Action
  ( Request
  , RequestContext
  , ActionM
  , Action

  , Response
  , response
  , emptyResponse
  , redirectRouteResponse
  , otherRouteResponse
  , forbiddenResponse
  , notFoundResponse
  , okResponse
  , result
  , maybeAction

  , module Databrary.Action.Route

  , withAuth
  , runActionRoute
  ) where

import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromMaybe)
import Network.HTTP.Types (Status, seeOther303, forbidden403, notFound404, ResponseHeaders, hLocation)
import qualified Network.Wai as Wai

import Databrary.Has (peeks)
import Databrary.HTTP.Request
import Databrary.Action.Types
import Databrary.Action.Response
import Databrary.Action.Route
import Databrary.Service.Types
import Databrary.HTTP.Route
import Databrary.View.Error

redirectRouteResponse :: Status -> ResponseHeaders -> Route r a -> a -> Request -> Response
redirectRouteResponse s h r a req =
  emptyResponse s ((hLocation, BSL.toStrict $ BSB.toLazyByteString $ actionURL (Just req) r a (Wai.queryString req)) : h)

otherRouteResponse :: ResponseHeaders -> Route r a -> a -> Request -> Response
otherRouteResponse = redirectRouteResponse seeOther303

forbiddenResponse :: RequestContext -> Response
forbiddenResponse = response forbidden403 [] . htmlForbidden

notFoundResponse :: RequestContext -> Response
notFoundResponse = response notFound404 [] . htmlNotFound

maybeAction :: Maybe a -> ActionM a
maybeAction (Just a) = return a
maybeAction Nothing = result =<< peeks notFoundResponse

runActionRoute :: RouteMap Action -> Service -> Wai.Application
runActionRoute rm rc req = runAction rc
  (fromMaybe (withoutAuth $ peeks notFoundResponse) (lookupRoute req rm))
  req
