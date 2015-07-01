{-# LANGUAGE OverloadedStrings #-}
module Databrary.Model.Funding.FundRef
  ( lookupFunderRef
  , searchFundRef
  ) where

import Control.Monad ((<=<), (>=>), guard, mfilter)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import qualified Data.Foldable as Fold
import Data.Function (on)
import qualified Data.HashMap.Strict as HM
import Data.List (stripPrefix, sortBy, nubBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.Ord (comparing, Down(..))
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HC
import Text.Read (readMaybe)

import Databrary.Ops
import qualified Databrary.JSON as JSON
import Databrary.HTTP.Client
import Databrary.Service.DB
import Databrary.Model.Id.Types
import Databrary.Model.GeoNames
import Databrary.Model.Funding

fundRefDOI :: String
fundRefDOI = "10.13039/"

-- Not quite like Data.CaseInsensitive (non-strict 'ciFolded')
data CI = CI
  { unCI :: !T.Text
  , ciFolded :: T.Text
  }

{-# NOINLINE toCI #-} -- toCaseFold's entire switch gets inlined!
toCI :: T.Text -> CI
toCI t = CI t (T.toCaseFold t)

instance IsString CI where
  fromString = toCI . fromString

onCI :: (T.Text -> T.Text -> a) -> CI -> CI -> a
onCI f = f `on` ciFolded

instance Eq CI where
  (==) = onCI (==)
  (/=) = onCI (/=)

annotateFunder :: Funder -> [T.Text] -> Maybe T.Text -> Funder
annotateFunder f [] Nothing = f
annotateFunder f@Funder{ funderName = n } a c = f{ funderName =
  maybe id (\cc -> (<> (", " <> cc))) (mfilter (not . noc . toCI) c)
  $ case unCI <$> nai' of
      [] -> "" -- impossible
      [nn] -> nn
      (nn:aa) -> nn <> " (" <> T.intercalate ", " aa <> ")"
  }
  where
  ni = toCI n
  ai = toCI <$> sortBy (comparing $ Down . T.length) a
  nai' = nubBy (onCI T.isInfixOf) $
    (case filter (T.isInfixOf `onCI` ni) ai of
      [] -> ni
      (lni:_) -> lni) : ai
  noc ci = toCI (geoName geoNameUS) == ci || any (T.isInfixOf `onCI` ci) nai'

parseFundRef :: JSON.Value -> JSON.Parser (Funder, Maybe (Id GeoName))
parseFundRef = JSON.withObject "fundref" $ \j -> do
  doi <- j JSON..: "id"
  fid <- maybe (fail $ "doi: " ++ doi) (return . Id) $ readMaybe =<< stripPrefix ("http://dx.doi.org/" ++ fundRefDOI) doi
  name <- label =<< j JSON..: "prefLabel"
  let alts = mapMaybe (JSON.parseMaybe (label <=< JSON.parseJSON)) $ case HM.lookup "altLabel" j of
        Just (JSON.Array v) -> V.toList v
        Just o -> [o]
        Nothing -> []
      geo = do 
        r <- JSON.parseMaybe ((JSON..: "country") >=> (JSON..: "resource")) j
        g <- parseGeoNameRef r
        guard (g /= geoNameId geoNameUS)
        return g
  return (annotateFunder (Funder fid name) alts Nothing, geo)
  where
  label j = j JSON..: "Label" >>= (JSON..: "literalForm") >>= (JSON..: "content")

lookupFundRef :: (HTTPClientM c m, MonadThrow m) => Id Funder -> m (Maybe Funder)
lookupFundRef fi = runMaybeT $ do
  req <- HC.parseUrl $ "http://data.fundref.org/fundref/funder/" ++ fundRefDOI ++ show fi
  j <- MaybeT $ httpRequestJSON req
  (f, gi) <- MaybeT $ return $ JSON.parseMaybe parseFundRef j
  g <- flatMapM lookupGeoName gi
  return $ annotateFunder f [] (geoName <$> g)

lookupFunderRef :: (MonadDB m, HTTPClientM c m, MonadThrow m) => Id Funder -> m (Maybe Funder)
lookupFunderRef fi = do
  f <- lookupFunder fi
  f `orElseM` do
    r <- lookupFundRef fi
    Fold.mapM_ addFunder r
    return r

parseFundRefs :: JSON.Value -> JSON.Parser [Funder]
parseFundRefs = JSON.withArray "fundrefs" $
  return . mapMaybe (JSON.parseMaybe pfr) . V.toList
  where
  pfr = JSON.withObject "fundref" $ \j -> do
    is <- j JSON..: "id"
    i <- maybe (fail "invalid id") (return . Id) $ readMaybe is
    name <- j JSON..: "value"
    alts <- j JSON..:? "other_names"
    country <- j JSON..:? "country"
    return $ annotateFunder (Funder i name) (fromMaybe [] alts) country

searchFundRef :: (HTTPClientM c m, MonadThrow m) => T.Text -> m [Funder]
searchFundRef q = do
  req <- HC.setQueryString [("q", Just $ TE.encodeUtf8 q)]
    <$> HC.parseUrl "http://search.crossref.org/funders"
  j <- httpRequestJSON req
  return $ fromMaybe [] $ JSON.parseMaybe parseFundRefs =<< j
