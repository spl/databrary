{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Search where

import Control.Monad (liftM2, when)
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)

import qualified Databrary.JSON as JSON
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
    (query, offset, limit) <- runForm Nothing $ do
        query <- ("query" .:> deformNonEmpty deform)
        (limit, offset) <- paginationForm
        return (query, offset, limit)
    jsonResp <- flatMapM (\q -> search q offset limit) query
    okResponse [] $ fromMaybe JSON.Null jsonResp

