{-# LANGUAGE GADTs, TupleSections, QuasiQuotes, TypeOperators #-}
module Databrary.HTTP.Path.Parser
  ( I.Isomorphism((:<->:))
  , PathParser(..)
  , (>$<)
  , (</>)
  , (</>>)
  , (</>>>)
  , (>/>)
  , (</<)
  , (|/|)
  , pathMaybe
  , (=/=)
  , parsePath
  , producePath
  , pathCases
  ) where

import Control.Applicative ((<|>))
import Control.Arrow (first, second)
import Control.Monad (guard)
import qualified Control.Invariant.Functor as Inv
import Control.Invariant.Monoidal
import qualified Data.Isomorphism as I
import Data.String (IsString(..))
import qualified Data.Text as T
import Data.Typeable (cast)

import Databrary.HTTP.Path.Types

parserUndef :: PathParser a -> a
parserUndef _ = undefined

data PathParser a where
  PathEmpty :: PathParser ()
  PathFail :: PathParser a
  PathAny :: PathParser Path
  PathFixed :: !T.Text -> PathParser ()
  PathParameter :: PathParameter a => PathParser a
  PathTrans :: (a I.<-> b) -> PathParser a -> PathParser b
  PathTuple :: PathParser a -> PathParser b -> PathParser (a, b)
  PathEither :: PathParser a -> PathParser b -> PathParser (Either a b)

instance Inv.Functor PathParser where
  fmap f (PathTrans g p) = PathTrans (f I.. g) p
  fmap f p = PathTrans f p

instance Monoidal PathParser where
  unit = PathEmpty
  (>*<) = PathTuple

instance MonoidalPlus PathParser where
  zero = PathFail
  (>|<) = PathEither

instance IsString (PathParser ()) where
  fromString = PathFixed . fromString

pathParse :: PathParser a -> Path -> Maybe (a, Path)
pathParse PathEmpty l = Just ((), l)
pathParse PathFail _ = Nothing
pathParse PathAny l = Just (l, [])
pathParse (PathFixed t) (a:l) = (, l) <$> guard (a == t)
pathParse PathParameter (a:l) = (, l) <$> pathParameter a
pathParse (PathTrans f p) a = first (I.isoTo f) <$> pathParse p a
pathParse (PathTuple p q) a = do
  (pr, a') <- pathParse p a
  first ((,) pr) <$> pathParse q a'
pathParse (PathEither p q) a = first Left <$> pathParse p a <|> first Right <$> pathParse q a
pathParse _ _ = Nothing

parsePath :: PathParser a -> Path -> Maybe a
parsePath p l = do
  (a, []) <- pathParse p l
  return a

producePath :: PathParser a -> a -> PathElements
producePath PathEmpty () = []
producePath PathFail _ = []
producePath PathAny l = [PathElementAny l]
producePath (PathFixed t) () = [PathElementFixed t]
producePath PathParameter a = [PathElementParameter a]
producePath (PathTrans f p) a = producePath p $ I.isoFrom f a
producePath (PathTuple p q) (a, b) = producePath p a ++ producePath q b
producePath (PathEither p _) (Left a) = producePath p a
producePath (PathEither _ p) (Right a) = producePath p a

infixr 2 </>, </>>, </>>>, >/>, </<
(</>) :: PathParser a -> PathParser b -> PathParser (a, b)
(</>) = (>*<)

(</>>) :: PathParser a -> PathParser (b, c) -> PathParser (a, b, c)
(</>>) l r = [I.isoCase|(a, (b, c)) <-> (a, b, c)|] >$< PathTuple l r

(</>>>) :: PathParser a -> PathParser (b, c, d) -> PathParser (a, b, c, d)
(</>>>) l r = [I.isoCase|(a, (b, c, d)) <-> (a, b, c, d)|] >$< PathTuple l r

(>/>) :: PathParser () -> PathParser a -> PathParser a
(>/>) = (>*)

(</<) :: PathParser a -> PathParser () -> PathParser a
(</<) = (*<)

infix 3 |/|, =/=
(|/|) :: PathParser a -> PathParser b -> PathParser (Either a b)
(|/|) = (>|<)

-- empty-biased version of 'Inv.possible'
pathMaybe :: PathParser a -> PathParser (Maybe a)
pathMaybe p = I.rgt >$< (PathEmpty |/| p)

(=/=) :: Eq a => a -> PathParser a -> PathParser a
(=/=) a p = I.fromMaybe a >$< pathMaybe p

pathCases :: PathParser a -> [([PathElement], PathElements -> Maybe (a, PathElements))]
pathCases PathEmpty = [([], Just . (,) ())]
pathCases PathFail = []
pathCases PathAny = [([PathElementAny undefined], rf)] where
  rf (PathElementAny p:l) = Just (p, l)
  rf _ = Nothing
pathCases (PathFixed t) = [([PathElementFixed t], rf)] where
  rf (PathElementFixed _:l) = Just ((), l)
  rf _ = Nothing
pathCases d@PathParameter = [([PathElementParameter (parserUndef d)], rf)] where
  rf (PathElementParameter v:l) = (, l) <$> cast v
  rf _ = Nothing
pathCases (PathTrans f p) = second (fmap (first $ I.isoTo f) .) <$> pathCases p
pathCases (PathTuple a b) = do
  (ae, arf) <- pathCases a
  (be, brf) <- pathCases b
  return (ae ++ be, \r -> do
    (av, ar) <- arf r
    (bv, br) <- brf ar
    return ((av, bv), br))
pathCases (PathEither a b) =
  (second (fmap (first Left)  .) <$> pathCases a) ++
  (second (fmap (first Right) .) <$> pathCases b)
