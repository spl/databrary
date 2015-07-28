{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Search where

import Control.Monad (when)
import Data.Maybe (fromMaybe)

import qualified Databrary.JSON as JSON
import Databrary.Model.Paginate
import Databrary.Search.Solr
import Databrary.Action
import Databrary.HTTP.Path.Parser
import Databrary.HTTP.Form.Deform
import Databrary.Controller.Form
import Databrary.Controller.Angular
import Databrary.Ops

postSearch :: AppRoute API
postSearch = action GET (pathAPI </< "search") $ \api -> withAuth $ do
    when (api == HTML) angular
    (query, p) <- runForm Nothing $ do
        query <- ("query" .:> deformNonEmpty deform)
        p <- paginateForm
        return (query, p)
    jsonResp <- flatMapM (\q -> search q (paginateOffset p) (paginateLimit p)) query
    okResponse [] $ fromMaybe JSON.Null jsonResp

