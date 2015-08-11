{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Search
  ( postSearch
  , getUpdateIndex
  , postUpdateIndex
  ) where

import Control.Monad (when)
import Data.Maybe (fromMaybe)

import Databrary.Ops
import Databrary.Has (peek, focusIO)
import qualified Databrary.JSON as JSON
import Databrary.Model.Paginate
import Databrary.Search.Solr
import Databrary.Search.Index
import Databrary.Action
import Databrary.HTTP.Path.Parser
import Databrary.HTTP.Form.Deform
import Databrary.Controller.Form
import Databrary.Controller.Angular
import Databrary.Controller.Permission
import Databrary.View.Search

postSearch :: AppRoute API
postSearch = action GET (pathAPI </< "search") $ \api -> withAuth $ do
  when (api == HTML) angular
  (query, p) <- runForm Nothing $ do
    query <- ("query" .:> deformNonEmpty deform)
    p <- paginateForm
    return (query, p)
  jsonResp <- flatMapM (\q -> search q (paginateOffset p) (paginateLimit p)) query
  okResponse [] $ fromMaybe JSON.Null jsonResp

getUpdateIndex :: AppRoute ()
getUpdateIndex = action GET ("search" >/> "index") $ \() -> withAuth $ do
  checkMemberADMIN
  blankForm htmlUpdateIndex

postUpdateIndex :: AppRoute ()
postUpdateIndex = action POST ("search" >/> "index") $ \() -> withAuth $ do
  checkMemberADMIN
  focusIO . updateIndex =<< peek
  okResponse [] ("done" :: String)
