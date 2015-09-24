{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Libs
  ( generateLib
  , webDeps
  , webLibs
  , webIncludes
  ) where

import Control.Monad (mzero)
import Data.Maybe (maybeToList)
import Data.List (stripPrefix)
import Data.String (fromString)
import System.FilePath ((</>), splitFileName, (<.>))

import Databrary.Web
import Databrary.Web.Types
import Databrary.Web.Generate

prefix :: FilePath
prefix = "bower_components"

jsDeps, jsIncludes, jsLibs, jsAll :: [(FilePath, FilePath)]
jsDeps = -- sourced on js pages
  [ ("jquery",              "jquery/dist")
  , ("angular",             "angular")
  , ("angular-route",       "angular-route")
  , ("ng-flow-standalone",  "ng-flow/dist")
  , ("lodash",              "lodash")
  ]
jsIncludes = -- included in app
  map (\n -> ("jquery.ui." ++ n, "jquery-ui/ui")) ["core", "widget", "mouse", "slider"]
  ++ [("slider", "angular-ui-slider/src")]
jsLibs = -- only linked
  [ ("jquery-ui",           "jquery-ui/ui/minified")
  , ("jquery.csv",          "jquery-csv/src")
  , ("pivot",               "pivottable/dist")
  ] 
jsAll = jsDeps ++ jsIncludes ++ jsLibs

extensions :: [FilePath]
extensions = ["js", "min.js", "min.map", "min.js.map", "css"]

generateLib :: WebGenerator
generateLib fo@(f, _)
  | ("lib/", l) <- splitFileName (webFileRel f)
  , [p] <- [ p | (b, p) <- jsAll, ('.':e) <- maybeToList (stripPrefix b l), e `elem` extensions ] =
    webLinkDataFile (prefix </> p </> l) fo
  | otherwise = mzero

webJS :: Bool -> [(FilePath, FilePath)] -> [WebFilePath]
webJS mn = map (fromString . ("lib" </>) . (<.> if mn then ".min.js" else ".js") . fst)

webDeps :: Bool -> [WebFilePath]
webDeps debug = webJS (not debug) jsDeps

webLibs :: [WebFilePath]
webLibs = webJS True (jsDeps ++ jsLibs) ++ ["lib/pivot.css"]

webIncludes :: [WebFilePath]
webIncludes = webJS False jsIncludes
