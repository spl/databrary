module Databrary.Solr.Util
  ( unCamel
  ) where

import Data.Char (isUpper, toLower)

unCamel :: String -> String
unCamel "" = ""
unCamel (c:s)
  | isUpper c = '_':toLower c:unCamel s
  | otherwise = c:unCamel s

