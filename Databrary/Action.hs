{-# LANGUAGE OverloadedStrings #-}
module Databrary.Action
  ( Request
  , ActionM
  , Action
  , MonadAction
  , AppRequest
  , AppActionM
  , AppAction
  , MonadAppAction

  , Response
  , returnResponse
  , emptyResponse
  , redirectRouteResponse
  , otherRouteResponse
  , forbiddenResponse
  , notFoundResponse
  , okResponse
  , result
  , guardAction
  , maybeAction

  , module Databrary.Action.Route
  , AppRoute

  , withAuth
  , runAppRoute
  ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Network.HTTP.Types (Status, ok200, seeOther303, forbidden403, notFound404, ResponseHeaders, hLocation)
import qualified Network.Wai as Wai

import Databrary.Has (peek, peeks)
import Databrary.HTTP.Request
import Databrary.Action.Types
import Databrary.Action.Response
import Databrary.Action.App
import Databrary.Action.Route
import Databrary.Service.Types
import Databrary.HTTP.Route
import {-# SOURCE #-} Databrary.View.Error

emptyResponse :: MonadAction q m => Status -> ResponseHeaders -> m Response
emptyResponse s h = returnResponse s h (mempty :: BSB.Builder)

redirectRouteResponse :: MonadAction c m => Status -> ResponseHeaders -> Route r a -> a -> m Response
redirectRouteResponse s h r a = do
  req <- peek
  emptyResponse s ((hLocation, BSL.toStrict $ BSB.toLazyByteString $ actionURL (Just req) r a (Wai.queryString req)) : h)

otherRouteResponse :: MonadAction c m => ResponseHeaders -> Route r a -> a -> m Response
otherRouteResponse = redirectRouteResponse seeOther303

forbiddenResponse :: MonadAppAction q m => m Response
forbiddenResponse = returnResponse forbidden403 [] =<< peeks htmlForbidden

notFoundResponse :: MonadAppAction q m => m Response
notFoundResponse = returnResponse notFound404 [] =<< peeks htmlNotFound

okResponse :: (MonadAction q m, ResponseData r) => ResponseHeaders -> r -> m Response
okResponse = returnResponse ok200

guardAction :: (MonadAction q m, MonadIO m) => Bool -> m Response -> m ()
guardAction True _ = return ()
guardAction False r = result =<< r

maybeAction :: (MonadAppAction q m, MonadIO m) => Maybe a -> m a
maybeAction (Just a) = return a
maybeAction Nothing = result =<< notFoundResponse

type AppRoute a = Route AppAction a

runAppRoute :: RouteMap AppAction -> Service -> Wai.Application
runAppRoute rm rc req = runApp rc
  (fromMaybe (withoutAuth notFoundResponse) (lookupRoute req rm))
  req
