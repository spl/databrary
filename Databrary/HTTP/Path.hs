{-# LANGUAGE OverloadedStrings #-}
module Databrary.HTTP.Path
  ( Path
  , elementsPath
  , showPathElements'
  , encodePathParser
  ) where

import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Text as T
import Data.Typeable (typeOf)

import Databrary.HTTP
import Databrary.HTTP.Path.Types
import Databrary.HTTP.Path.Parser

elementPath :: PathElement -> Path
elementPath (PathElementFixed t) = [t]
elementPath (PathElementParameter a) = [parameterPath a]
elementPath (PathElementAny p) = p

elementsPath :: PathElements -> Path
elementsPath = concatMap elementPath

elementText' :: PathElement -> T.Text
elementText' (PathElementFixed t) = t
elementText' (PathElementParameter a) = T.pack $ show $ typeOf a
elementText' (PathElementAny _) = "*"

showPathElements' :: PathElements -> String
showPathElements' = BSLC.unpack . BSB.toLazyByteString . encodePathSegments' . map elementText'

encodePathParser :: PathParser a -> a -> BSB.Builder
encodePathParser p a = encodePathSegments' $ elementsPath $ producePath p a
