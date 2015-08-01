{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Libs
  ( generateLib
  , allWebLibs
  ) where

import Control.Monad (mzero)
import Data.String (fromString)
import System.FilePath ((</>), splitFileName, (<.>), splitExtensions)

import Databrary.Web
import Databrary.Web.Types
import Databrary.Web.Generate

jsLibs :: [(FilePath, FilePath)]
jsLibs =
  [ ("jquery",              "bower_components/jquery/dist")
  , ("jquery-ui",           "bower_components/jquery-ui")
  , ("angular",             "bower_components/angular")
  , ("angular-route",       "bower_components/angular-route")
  , ("ng-flow-standalone",  "bower_components/ng-flow/dist")
  , ("lodash",              "bower_components/lodash")
  ]

generateLib :: WebGenerator
generateLib fo@(f, _)
  | ("lib/", l) <- splitFileName (webFileRel f)
  , (b, e) <- splitExtensions l
  , e `elem` [".js", ".min.js", ".min.map", ".min.js.map"]
  , Just p <- lookup b jsLibs = webLinkDataFile (p </> l) fo
  | otherwise = mzero

allWebLibs :: Bool -> [WebFilePath]
allWebLibs debug = map (fromString . ("lib" </>) . (<.> if debug then ".js" else ".min.js") . fst) jsLibs
