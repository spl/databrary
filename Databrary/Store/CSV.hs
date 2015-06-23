module Databrary.Store.CSV
  ( buildCSV
  ) where

import Data.List (intersperse)
import Data.Monoid ((<>), mconcat)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as B

inter :: B.Builder -> [B.Builder] -> B.Builder
inter d = mconcat . intersperse d

csvCell :: T.Text -> B.Builder
csvCell t
  | T.any (`elem` "\",\r\n") t = q <> inter (q <> q) (map B.fromText $ T.split ('"' ==) t) <> q
  | otherwise = B.fromText t
  where q = B.singleton '"'

csvRow :: [T.Text] -> B.Builder
csvRow r = inter (B.singleton ',') (map csvCell r) <> B.singleton '\n'

buildCSV :: [[T.Text]] -> B.Builder
buildCSV = mconcat . map csvRow
