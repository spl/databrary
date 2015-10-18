{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Databrary.Model.Citation.Types
  ( Citation(..)
  ) where

import Control.Applicative ((<|>))
import Data.Int (Int16)
import Data.Maybe (catMaybes)
import qualified Data.Text as T

import qualified Databrary.JSON as JSON
import Databrary.Model.URL (URI)

data Citation = Citation
  { citationHead :: T.Text
  , citationURL :: Maybe URI
  , citationYear :: Maybe Int16
  , citationTitle :: Maybe T.Text
  }

instance Monoid Citation where
  mempty = Citation
    { citationHead = T.empty
    , citationURL = Nothing
    , citationYear = Nothing
    , citationTitle = Nothing
    }
  mappend a b = Citation
    { citationHead = if T.null (citationHead a) then citationHead b else citationHead a
    , citationURL = citationURL a <|> citationURL b
    , citationYear = citationYear a <|> citationYear b
    , citationTitle = citationTitle a <|> citationTitle b
    }

instance JSON.ToJSON Citation where
  toJSON Citation{..} = JSON.Object $ JSON.object $ catMaybes
    [ Just $ "head" JSON..= citationHead
    , ("title" JSON..=) <$> citationTitle
    , ("url" JSON..=) <$> citationURL
    , ("year" JSON..=) <$> citationYear
    ]
