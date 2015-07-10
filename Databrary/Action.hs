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
  , AuthRequest
  , AuthActionM
  , AuthAction
  , MonadAuthAction

  , Response
  , returnResponse
  , emptyResponse
  , redirectRouteResponse
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
import Network.HTTP.Types (Status, ok200, seeOther303, forbidden403, notFound404, ResponseHeaders, hLocation, Query)
import qualified Network.Wai as Wai

import Databrary.Has (peek, peeks)
import Databrary.HTTP.Request
import Databrary.Action.Types
import Databrary.Action.Response
import Databrary.Action.App
import Databrary.Action.Auth
import Databrary.Action.Route
import Databrary.Service.Types
import Databrary.HTTP.Route
import {-# SOURCE #-} Databrary.View.Error

emptyResponse :: MonadAction q m => Status -> ResponseHeaders -> m Response
emptyResponse s h = returnResponse s h (mempty :: BSB.Builder)

redirectRouteResponse :: MonadAction c m => ResponseHeaders -> Route r a -> a -> Query -> m Response
redirectRouteResponse h r a q = do
  req <- peek
  emptyResponse seeOther303 ((hLocation, BSL.toStrict $ BSB.toLazyByteString $ actionURL (Just req) r a q) : h)

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
  (fromMaybe notFoundResponse (lookupRoute req rm))
  req
