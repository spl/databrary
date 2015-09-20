{-# LANGUAGE ExistentialQuantification, RecordWildCards, ImpredicativeTypes #-}
module Databrary.HTTP.Route
  ( Route(..)
  , route
  , RouteMap
  , fromRouteList
  , lookupRoute
  , routeURL
  , routeURI
  ) where

import Prelude hiding (lookup)

import Control.Applicative ((<$>))
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.HashMap.Strict as HM
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Network.HTTP.Types (Method)
import Network.URI (URI(..), nullURI)
import qualified Network.Wai as Wai

import Databrary.Iso.Types (Invariant(..))
import Databrary.HTTP
import Databrary.HTTP.Request
import Databrary.HTTP.Path.Types
import qualified Databrary.HTTP.Path.Map as PM
import Databrary.HTTP.Path.Parser
import Databrary.HTTP.Path

data Route r a = Route
  { routeMethod :: !Method
  , routeMultipart :: !Bool
  , routePath :: PathParser a
  , routeAction :: a -> r
  }

instance Invariant (Route r) where
  invMap f g r = r
    { routePath = invMap f g $ routePath r
    , routeAction = routeAction r . g
    }

type RouteResult r = PathElements -> r

data RouteCase r = RouteCase
  { _routeMethod :: !Method
  , _routeElements :: PathElements
  , _routeResult :: RouteResult r
  }

route :: Route r a -> [RouteCase r]
route Route{ routeMethod = m, routePath = p, routeAction = f } = cf <$> pathCases p where
  cf (e, rf) = RouteCase m e $ \r -> fromMaybe (error $ "route: " ++ (BSLC.unpack $ BSB.toLazyByteString $ encodePathSegments' $ elementsPath r)) $ do
    (v, []) <- rf r
    return $ f v

newtype RouteMap r = RouteMap (HM.HashMap Method (PM.PathMap (RouteResult r)))

empty :: RouteMap a
empty = RouteMap HM.empty

insert :: RouteCase r -> RouteMap r -> RouteMap r
insert (RouteCase a p r) (RouteMap m) = RouteMap $
  HM.insertWith (const $ PM.insert p r) a (PM.singleton p r) m

fromRouteList :: [[RouteCase r]] -> RouteMap r
fromRouteList = foldl' (flip insert) empty . concat

lookupRoute :: Wai.Request -> RouteMap r -> Maybe r
lookupRoute q (RouteMap m) = fmap (uncurry (flip ($))) $
  PM.lookup (Wai.pathInfo q) =<< HM.lookup (Wai.requestMethod q) m

routeURL :: Maybe Wai.Request -> Route r a -> a -> BSB.Builder
routeURL req Route{ routePath = p } a =
  maybe id ((<>) . BSB.byteString . requestHost) req $ encodePathSegments' $ elementsPath $ producePath p a

routeURI :: Maybe Wai.Request -> Route r a -> a -> URI
routeURI req Route{ routePath = p } a = (maybe nullURI requestURI req)
  { uriPath = BSLC.unpack $ BSB.toLazyByteString $ encodePathSegments' $ elementsPath $ producePath p a
  , uriQuery = ""
  }
