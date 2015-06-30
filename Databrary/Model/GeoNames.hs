{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module Databrary.Model.GeoNames
  ( GeoName(..)
  , MonadHasGeoName
  , geoNameUS
  , parseGeoNameRef
  , lookupGeoName
  ) where

import Control.Applicative ((<$>))
import Control.Monad (guard)
import Control.Monad.Catch (MonadThrow)
import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import qualified Network.HTTP.Client as HC

import Databrary.Has (makeHasRec)
import qualified Databrary.JSON as JSON
import Databrary.HTTP.Client
import Databrary.Model.Id.Types

type instance IdType GeoName = Int64

data GeoName = GeoName
  { geoNameId :: !(Id GeoName)
  , geoName :: !T.Text
  }

makeHasRec ''GeoName ['geoNameId]

geoNameUS :: GeoName
geoNameUS = GeoName
  { geoNameId = Id 6252001
  , geoName = "United States"
  }

parseGeoNameRef :: String -> Maybe (Id GeoName)
parseGeoNameRef s = listToMaybe $ do
  (i, r) <- reads $ fromMaybe s (stripPrefix "http://sws.geonames.org/" s)
  guard (null r || r == "/")
  return $ Id i

parseGeoName :: JSON.Value -> JSON.Parser GeoName
parseGeoName = JSON.withObject "geoname" $ \j -> do
  i <- j JSON..: "geonameId"
  n <- j JSON..: "name"
  return GeoName
    { geoNameId = Id i
    , geoName = n
    }

lookupGeoName :: (HTTPClientM c m, MonadThrow m) => Id GeoName -> m (Maybe GeoName)
lookupGeoName (Id i) = do
  req <- HC.setQueryString [("geonameId", Just $ BSC.pack $ show i), ("username", Just "databrary")]
    <$> HC.parseUrl "http://api.geonames.org/getJSON"
  j <- httpRequestJSON req
  return $ JSON.parseMaybe parseGeoName =<< j
