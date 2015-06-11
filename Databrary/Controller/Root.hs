{-# LANGUAGE CPP, OverloadedStrings #-}
module Databrary.Controller.Root
  ( viewRoot
  , viewConstants
  , viewRobotsTxt
  ) where

import Control.Monad (when)
import qualified Data.Aeson.Types as JSON
import qualified Data.Text as T

import Databrary.Has (peek)
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Controller.Angular
import Databrary.View.Root
import Databrary.Web.Constants

viewRoot :: AppRoute API
viewRoot = action GET pathAPI $ \api -> withAuth $ do
  when (api == HTML) angular
  case api of
    JSON -> okResponse [] JSON.emptyObject
    HTML -> okResponse [] . htmlRoot =<< peek

viewConstants :: AppRoute ()
viewConstants = action GET (pathJSON >/> "constants") $ \() ->
  okResponse [] constantsJSON

viewRobotsTxt :: AppRoute ()
viewRobotsTxt = action GET "robots.txt" $ \() -> do
  okResponse [] (
#if defined(DEVEL) || defined(SANDBOX)
    "User-agent: *\nDisallow: /\n"
#else
    ""
#endif
    :: T.Text)
