{-# LANGUAGE ExistentialQuantification, RecordWildCards, ImpredicativeTypes, GeneralizedNewtypeDeriving #-}
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

import qualified Control.Invariant.Functor as Inv
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Isomorphism as I
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Network.HTTP.Types (StdMethod)
import Network.URI (URI(..), nullURI)
import qualified Network.Wai as Wai

import Databrary.HTTP
import Databrary.HTTP.Request
import Databrary.HTTP.Path.Types
import qualified Databrary.HTTP.Path.Map as PM
import Databrary.HTTP.Path.Parser
import Databrary.HTTP.Path
import qualified Databrary.HTTP.Method.Map as MM

data Route r a = Route
  { routeMethod :: !StdMethod
  , routeMultipart :: !Bool
  , routePath :: PathParser a
  , routeAction :: a -> r
  }

instance Inv.Functor (Route r) where
  fmap f r = r
    { routePath = Inv.fmap f $ routePath r
    , routeAction = routeAction r . I.isoFrom f
    }

type RouteResult r = PathElements -> r

data RouteCase r = RouteCase
  { _routeMethod :: !StdMethod
  , _routeElements :: PathElements
  , _routeResult :: RouteResult r
  }

route :: Route r a -> [RouteCase r]
route Route{ routeMethod = m, routePath = p, routeAction = f } = cf <$> pathCases p where
  cf (e, rf) = RouteCase m e $ \r -> fromMaybe (error $ "route: " ++ (BSLC.unpack $ BSB.toLazyByteString $ encodePathSegments' $ elementsPath r)) $ do
    (v, []) <- rf r
    return $ f v

newtype RouteMap r = RouteMap (PM.PathMap (MM.MethodMap (RouteResult r)))
  deriving (Monoid)

singleton :: RouteCase r -> RouteMap r
singleton (RouteCase a p r) = RouteMap $
  PM.singleton p (MM.singleton a r)

fromRouteList :: [[RouteCase r]] -> RouteMap r
fromRouteList = foldMap singleton . concat

lookupRoute :: Wai.Request -> RouteMap r -> Either [StdMethod] r
lookupRoute q (RouteMap m) = do
  (p, mm) <- maybe (Left []) Right $ PM.lookup (Wai.pathInfo q) m
  r <- MM.lookup (Wai.requestMethod q) mm
  return $ r p

routeBuilder :: Route r a -> a -> BSB.Builder
routeBuilder Route{ routePath = p } a =
  encodePathSegments' $ elementsPath $ producePath p a

routeURL :: Maybe Wai.Request -> Route r a -> a -> BSB.Builder
routeURL req r a =
  maybe id ((<>) . BSB.byteString . requestHost) req $ routeBuilder r a

routeURI :: Maybe Wai.Request -> Route r a -> a -> URI
routeURI req r a = (maybe nullURI requestURI req)
  { uriPath = BSLC.unpack $ BSB.toLazyByteString $ routeBuilder r a
  , uriQuery = ""
  }
