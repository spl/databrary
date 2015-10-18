{-# LANGUAGE GADTs, TupleSections, QuasiQuotes #-}
module Databrary.HTTP.Path.Parser
  ( PathParser(..)
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
import Data.String (IsString(..))
import qualified Data.Text as T
import Data.Typeable (cast)

import qualified Databrary.Iso as I
import Databrary.Iso.TH
import Databrary.HTTP.Path.Types

parserUndef :: PathParser a -> a
parserUndef _ = undefined

data PathParser a where
  PathEmpty :: PathParser ()
  PathAny :: PathParser Path
  PathFixed :: !T.Text -> PathParser ()
  PathParameter :: PathParameter a => PathParser a
  PathTrans :: (a -> b) -> (b -> a) -> PathParser a -> PathParser b
  PathTuple :: PathParser a -> PathParser b -> PathParser (a, b)
  PathEither :: PathParser a -> PathParser b -> PathParser (Either a b)

instance I.Invariant PathParser where
  invMap f' g' (PathTrans f g p) = PathTrans (f' . f) (g . g') p
  invMap f g p = PathTrans f g p

instance IsString (PathParser ()) where
  fromString = PathFixed . fromString

pathParse :: PathParser a -> Path -> Maybe (a, Path)
pathParse PathEmpty l = Just ((), l)
pathParse PathAny l = Just (l, [])
pathParse (PathFixed t) (a:l) = (, l) <$> guard (a == t)
pathParse PathParameter (a:l) = (, l) <$> pathParameter a
pathParse (PathTrans f _ p) a = first f <$> pathParse p a
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
producePath PathAny l = [PathElementAny l]
producePath (PathFixed t) () = [PathElementFixed t]
producePath PathParameter a = [PathElementParameter a]
producePath (PathTrans _ g p) a = producePath p $ g a
producePath (PathTuple p q) (a, b) = producePath p a ++ producePath q b
producePath (PathEither p _) (Left a) = producePath p a
producePath (PathEither _ p) (Right a) = producePath p a

infixr 2 </>, </>>, </>>>, >/>, </<
(</>) :: PathParser a -> PathParser b -> PathParser (a, b)
(</>) = PathTuple

(</>>) :: PathParser a -> PathParser (b, c) -> PathParser (a, b, c)
(</>>) l r = [iso|(a, (b, c)) <-> (a, b, c)|] I.<$> PathTuple l r

(</>>>) :: PathParser a -> PathParser (b, c, d) -> PathParser (a, b, c, d)
(</>>>) l r = [iso|(a, (b, c, d)) <-> (a, b, c, d)|] I.<$> PathTuple l r

(>/>) :: PathParser () -> PathParser a -> PathParser a
(>/>) a b = I.snd I.<$> PathTuple a b

(</<) :: PathParser a -> PathParser () -> PathParser a
(</<) a b = I.fst I.<$> PathTuple a b

infix 3 |/|, =/=
(|/|) :: PathParser a -> PathParser b -> PathParser (Either a b)
(|/|) = PathEither

pathMaybe :: PathParser a -> PathParser (Maybe a)
pathMaybe p = I.rgt I.<$> (PathEmpty |/| p)

(=/=) :: Eq a => a -> PathParser a -> PathParser a
(=/=) a p = I.defaultEq a I.<$> pathMaybe p

pathCases :: PathParser a -> [([PathElement], PathElements -> Maybe (a, PathElements))]
pathCases PathEmpty = [([], Just . (,) ())]
pathCases PathAny = [([PathElementAny undefined], rf)] where
  rf (PathElementAny p:l) = Just (p, l)
  rf _ = Nothing
pathCases (PathFixed t) = [([PathElementFixed t], rf)] where
  rf (PathElementFixed _:l) = Just ((), l)
  rf _ = Nothing
pathCases d@PathParameter = [([PathElementParameter (parserUndef d)], rf)] where
  rf (PathElementParameter v:l) = (, l) <$> cast v
  rf _ = Nothing
pathCases (PathTrans f _ p) = second (fmap (first f) .) <$> pathCases p
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
