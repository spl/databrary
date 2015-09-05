{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Routes
  ( generateRoutesJS
  , jsRoute
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))
import System.IO (withBinaryFile, IOMode(WriteMode), hPutStr, hPutStrLn, hFlush)

import Databrary.JSON (quoteByteString)
import Databrary.HTTP.Path.JS
import Databrary.HTTP.Route
import Databrary.Web.Types
import Databrary.Web.Generate

import {-# SOURCE #-} Databrary.Routes.JS

jsRoute :: BS.ByteString -> Route r a -> a -> B.Builder
jsRoute n r v = B.char8 '\n' <> quoteByteString '"' n
  <> B.string8 ":{method:" <> quoteByteString '"' (routeMethod r)
  <> B.string8 ",route:" <> jsPath (routePath r) v <> B.string8 "},"

generateRoutesJS :: WebGenerator
generateRoutesJS = staticWebGenerate $ \f ->
  withBinaryFile f WriteMode $ \h -> do
    hPutStrLn h "'use strict';"
    hPutStr h "app.constant('routeData',{"
    hFlush h                 -- need this
    B.hPutBuilder h jsRoutes -- or this hangs
    hPutStrLn h "});"
