{-# LANGUAGE GADTs, KindSignatures, DataKinds, TypeOperators, DeriveDataTypeable #-}
module Databrary.HTTP.Path.Map
  ( PathMap
  , PathMapConflict(..)
  , singleton
  , lookup
  , insert
  ) where

import Prelude hiding (lookup, null)

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Exception (Exception, throw)
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Monoid (Monoid(..), (<>))
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import Data.Typeable (Typeable, typeRep)

import Databrary.HTTP.Path.Types

data PathParameterRep where
  PathParameterRep :: PathParameter a => !(Proxy a) -> PathParameterRep

instance Eq PathParameterRep where
  PathParameterRep a == PathParameterRep b = typeRep a == typeRep b

instance Ord PathParameterRep where
  PathParameterRep a `compare` PathParameterRep b = typeRep a `compare` typeRep b

instance Hashable PathParameterRep where
  hashWithSalt s (PathParameterRep d) = hashWithSalt s $ typeRep d

proxy :: a -> Proxy a
proxy _ = Proxy

pathParameterRep :: PathParameter a => a -> PathParameterRep
pathParameterRep = PathParameterRep . proxy

data PathMap a
  = PathMap
  { pathMapNull :: !(Maybe a)
  , pathMapFixed :: !(HM.HashMap T.Text (PathMap a))
  , pathMapParameter :: !(M.Map PathParameterRep (PathMap a))
  }
  | PathMapAny
  { _pathMapAny :: a
  }

data PathMapConflict = PathMapConflict deriving (Typeable)

instance Show PathMapConflict where
  show _ = "PathMapConflict"

instance Exception PathMapConflict

empty :: PathMap a
empty = PathMap Nothing HM.empty M.empty

null :: PathMap a -> Bool
null (PathMap Nothing f d) = HM.null f && M.null d
null _ = False

lookup :: Path -> PathMap a -> Maybe (PathElements, a)
lookup [] (PathMap n _ _) = (,) [] <$> n
lookup (e:p) m@(PathMap _ f d)
  | Just m' <- HM.lookup e f = first (PathElementFixed e :) <$> lookup p m'
  | T.null e = lookup p m
  | otherwise = ld (M.toList d) where
  ld [] = Nothing
  ld ((PathParameterRep r,m'):l)
    | Just a <- pathParameterAs r e = first (PathElementParameter a :) <$> lookup p m'
    | otherwise = ld l
lookup p (PathMapAny a) = Just ([PathElementAny p], a)

singleton :: PathElements -> a -> PathMap a
singleton [] a = PathMap (Just a) HM.empty M.empty
singleton (PathElementFixed e:l) a = PathMap Nothing (HM.singleton e (singleton l a)) M.empty
singleton (PathElementParameter e:l) a = PathMap Nothing HM.empty (M.singleton (pathParameterRep e) (singleton l a))
singleton (PathElementAny _:_) a = PathMapAny a

unionAny :: a -> PathMap a -> PathMap a
unionAny a m
  | null m = PathMapAny a
  | otherwise = throw PathMapConflict

union :: Monoid a => PathMap a -> PathMap a -> PathMap a
union (PathMap n1 f1 d1) (PathMap n2 f2 d2) =
  PathMap (n1 <> n2) (HM.unionWith union f1 f2) (M.unionWith union d1 d2)
union (PathMapAny a) (PathMapAny b) = PathMapAny (a <> b)
union (PathMapAny a) m = unionAny a m
union m (PathMapAny a) = unionAny a m

insert :: Monoid a => PathElements -> a -> PathMap a -> PathMap a
insert [] a p@PathMap{ pathMapNull = n } = p{ pathMapNull = Just (maybe id (<>) n a) }
insert (PathElementFixed e:l) a p@PathMap{ pathMapFixed = f } =
  p{ pathMapFixed = HM.insertWith (const $ insert l a) e (singleton l a) f }
insert (PathElementParameter e:l) a p@PathMap{ pathMapParameter = d } =
  p{ pathMapParameter = M.insertWith (const $ insert l a) (pathParameterRep e) (singleton l a) d }
insert (PathElementAny _:_) a m = unionAny a m
insert _ _ _ = throw PathMapConflict

instance Monoid a => Monoid (PathMap a) where
  mempty = empty
  mappend = union
