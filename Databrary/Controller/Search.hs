{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Search
  ( postSearch
  , getUpdateIndex
  , postUpdateIndex
  ) where

import Control.Monad (when)
import Data.Maybe (fromMaybe)

import Databrary.Ops
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

postSearch :: ActionRoute API
postSearch = action GET (pathAPI </< "search") $ \api -> withAuth $ do
  when (api == HTML) angular
  (query, p) <- runForm Nothing $ do
    query <- ("query" .:> deformNonEmpty deform)
    p <- paginateForm
    return (query, p)
  jsonResp <- flatMapM (\q -> search q p) query
  return $ okResponse [] $ fromMaybe JSON.Null jsonResp

getUpdateIndex :: ActionRoute ()
getUpdateIndex = action GET ("search" >/> "index") $ \() -> withAuth $ do
  checkMemberADMIN
  peeks $ blankForm . htmlUpdateIndex

postUpdateIndex :: ActionRoute ()
postUpdateIndex = action POST ("search" >/> "index") $ \() -> withAuth $ do
  checkMemberADMIN
  focusIO . updateIndex =<< peek
  return $ okResponse [] ("done" :: String)
