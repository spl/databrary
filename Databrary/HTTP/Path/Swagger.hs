module Databrary.HTTP.Path.Swagger
  ( swaggerPath
  ) where

import Data.Monoid ((<>))
import qualified Data.Text as T

import Databrary.HTTP.Path.Types
import Databrary.HTTP.Path.Parser

parameter :: T.Text -> T.Text
parameter p = '{' `T.cons` p `T.snoc` '}'

pathElements :: PathElements -> [T.Text] -> T.Text
pathElements [] [] = T.empty
pathElements (PathElementFixed t : l) p = '/' `T.cons` t <> pathElements l p
pathElements (PathElementParameter _ : l) (a:p) = '/' `T.cons` parameter a <> pathElements l p
pathElements (PathElementAny _ : l) (a:p) = '/' `T.cons` parameter a <> pathElements l p
pathElements _ _ = error "swaggerPath parameter mismatch"

swaggerPath :: PathParser a -> a -> [T.Text] -> T.Text
swaggerPath p = pathElements . producePath p
