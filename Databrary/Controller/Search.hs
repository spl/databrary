{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Databrary.Controller.Search
  ( postSearch
  , viewUpdateIndex
  , postUpdateIndex
  ) where

import Control.Applicative (Applicative, (<$>), (<*>), (<|>))
import Control.Monad (when)
import Data.Maybe (fromMaybe)

import Databrary.Has
import qualified Databrary.JSON as JSON
import Databrary.Solr.Search
import Databrary.Solr.Index
import Databrary.Action
import Databrary.HTTP.Path.Parser
import Databrary.HTTP.Form.Deform
import Databrary.Controller.Form
import Databrary.Controller.Angular
import Databrary.Controller.Permission
import Databrary.View.Search

searchForm :: (Applicative m, Monad m) => DeformT f m SearchQuery
searchForm = SearchQuery
  <$> ("query" .:> deformNonEmpty deform)
  <*> ("terms" .:> withSubDeforms (\k -> (view k, ) <$> deform))
  <*> ("volume" .:> fromMaybe SearchVolumes <$> deformOptional ((SearchVolume <$> deform) <|> (sv <$> deform)))
  <*> paginateForm
  where
  sv False = SearchParties
  sv True = SearchVolumes

postSearch :: ActionRoute API
postSearch = action GET (pathAPI </< "search") $ \api -> withAuth $ do
  when (api == HTML) angular
  q <- runForm Nothing searchForm
  jsonResp <- search q
  return $ okResponse [] $ fromMaybe JSON.Null jsonResp

viewUpdateIndex :: ActionRoute ()
viewUpdateIndex = action GET ("search" >/> "index") $ \() -> withAuth $ do
  checkMemberADMIN
  peeks $ blankForm . htmlUpdateIndex

postUpdateIndex :: ActionRoute ()
postUpdateIndex = action POST ("search" >/> "index") $ \() -> withAuth $ do
  checkMemberADMIN
  focusIO . updateIndex =<< peek
  return $ okResponse [] ("done" :: String)
