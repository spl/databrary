{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Databrary.Controller.Search
  ( postSearch
  , viewUpdateIndex
  , postUpdateIndex
  ) where

import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Network.HTTP.Types.Status (badRequest400)

import Databrary.Has
import Databrary.Model.Id.Types
import Databrary.Model.Metric
import Databrary.Solr.Search
import Databrary.Solr.Index
import Databrary.Action
import Databrary.HTTP.Path.Parser
import Databrary.HTTP.Form (FormKey(..))
import Databrary.HTTP.Form.Deform
import Databrary.Controller.Form
import Databrary.Controller.Angular
import Databrary.Controller.Permission
import Databrary.View.Search

searchForm :: DeformActionM f SearchQuery
searchForm = SearchQuery
  <$> ("q" .:> deformNonEmpty deform)
  <*> ("f" .:> withSubDeforms (\k -> (view k, ) <$> deform))
  <*> ("m" .:> withSubDeforms (\k -> (,)
    <$> (either deformError' return $ maybe (Left "Metric ID not found") Right . getMetric . Id =<< case k of
      FormField t -> textInteger t
      FormIndex i -> Right (fromIntegral i))
    <*> deform))
  <*> ("volume" .:> fromMaybe SearchVolumes <$> deformOptional ((SearchVolume <$> deform) <|> (sv <$> deform)))
  <*> paginateForm
  where
  sv False = SearchParties
  sv True = SearchVolumes

postSearch :: ActionRoute API
postSearch = action GET (pathAPI </< "search") $ \api -> withAuth $ do
  when (api == HTML) angular
  q <- runForm Nothing searchForm
  maybe (emptyResponse badRequest400 []) (okResponse []) <$> search q

viewUpdateIndex :: ActionRoute ()
viewUpdateIndex = action GET ("search" >/> "index") $ \() -> withAuth $ do
  checkMemberADMIN
  peeks $ blankForm . htmlUpdateIndex

postUpdateIndex :: ActionRoute ()
postUpdateIndex = action POST ("search" >/> "index") $ \() -> withAuth $ do
  checkMemberADMIN
  focusIO . updateIndex =<< peek
  return $ okResponse [] ("done" :: String)
