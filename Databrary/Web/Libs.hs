{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Libs
  ( generateLib
  , webLibs
  , webIncludes
  ) where

import Control.Applicative ((<$), (<|>))
import Control.Monad (guard, mzero)
import Data.String (fromString)
import System.FilePath ((</>), splitFileName, (<.>), splitExtensions, splitExtension)

import Databrary.Web
import Databrary.Web.Types
import Databrary.Web.Generate

prefix :: FilePath
prefix = "bower_components"

jsLibs, jsIncludes, jsAll :: [(FilePath, FilePath)]
jsAll = jsLibs ++ jsIncludes
jsLibs =
  [ ("jquery",              "jquery/dist")
  , ("angular",             "angular")
  , ("angular-route",       "angular-route")
  , ("ng-flow-standalone",  "ng-flow/dist")
  , ("lodash",              "lodash")
  ]
jsIncludes =
  map (\n -> ("jquery.ui." ++ n, "jquery-ui/ui")) ["core", "widget", "mouse", "slider"]
  ++ [("slider", "angular-ui-slider/src")]

generateLib :: WebGenerator
generateLib fo@(f, _)
  | ("lib/", l) <- splitFileName (webFileRel f)
  , Just b <- fgs (`elem` [".js", ".min.js", ".min.map", ".min.js.map"]) (splitExtensions l)
      <|> fgs (== ".js") (splitExtension l)
  , Just p <- lookup b jsAll = webLinkDataFile (prefix </> p </> l) fo
  | otherwise = mzero
  where fgs p (a, b) = a <$ guard (p b)

webJS :: Bool -> [(FilePath, FilePath)] -> [WebFilePath]
webJS mn = map (fromString . ("lib" </>) . (<.> if mn then ".min.js" else ".js") . fst)

webLibs :: Bool -> [WebFilePath]
webLibs debug = webJS (not debug) jsLibs

webIncludes :: [WebFilePath]
webIncludes = webJS False jsIncludes
