module Databrary.HTTP.Path.JS
  ( jsPath
  ) where

import Control.Arrow ((***), second)
import Control.Monad (liftM2)
import qualified Data.ByteString.Builder as B
import Data.Char (isAscii, isAlphaNum, toLower)
import Data.List (intersperse)
import Data.Monoid ((<>), mempty, mconcat)
import qualified Data.Text.Encoding as TE
import Data.Typeable (typeOf)

import Databrary.JSON (escapeByteString)
import Databrary.HTTP.Path.Types
import Databrary.HTTP.Path.Parser

elementArgs :: PathElements -> ([B.Builder], B.Builder)
elementArgs [] = ([], B.string8 "\"/\"")
elementArgs el = second (\r -> bq <> r <> bq) $ ea 0 el where
  ea i (PathElementFixed t:l) = second ((bs <> escapeByteString jq (TE.encodeUtf8 t)) <>) $ ea i l
  ea i (e:l) = (a :) *** (mconcat [bs, bq, bp, a, bp, bq] <>) $ ea (succ i) l
    where a = B.string8 (av e) <> B.intDec i
  ea _ [] = ([], mempty)
  av (PathElementParameter a) = tl $ filter (liftM2 (&&) isAscii isAlphaNum) $ show $ typeOf a
  av _ = "path"
  tl [] = "a"
  tl (c:l) = toLower c : l
  bq = B.char8 jq
  bp = B.char8 '+'
  bs = B.char8 '/'
  jq = '"'

jsPath :: PathParser a -> a -> B.Builder
jsPath p a = B.string8 "function(" <> mconcat (intersperse (B.char8 ',') args) <>
  B.string8 "){return " <> expr <> B.string8 ";}"
  where
  el = producePath p a
  (args, expr) = elementArgs el
