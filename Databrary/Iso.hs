{-# LANGUAGE TypeOperators, QuasiQuotes #-}
module Databrary.Iso
  ( module Databrary.Iso.Types
  , (C.>>>)
  , (C.<<<)
  , invert
  , first
  , second
  , (***)
  , left
  , right
  , (+++)

  , not
  , swap
  , constant
  , fst
  , snd
  , isJust
  , isNothing
  , switch
  , isLeft
  , isRight
  , lft
  , rgt
  , cons
  , curried
  , defaultEq
  ) where

import Prelude hiding (not, fst, snd)

import qualified Control.Arrow as A
import qualified Control.Category as C
import Data.Maybe (fromMaybe)

import Databrary.Iso.Types
import Databrary.Iso.TH

invert :: a <-> b -> b <-> a
invert (f :<->: g) = g :<->: f

first :: (a <-> b) -> ((a, c) <-> (b, c))
first (f :<->: g) = A.first f :<->: A.first g

second :: (a <-> b) -> ((c, a) <-> (c, b))
second (f :<->: g) = A.second f :<->: A.second g

infixr 3 ***
(***) :: (a <-> b) -> (a' <-> b') -> ((a, a') <-> (b, b'))
(f :<->: g) *** (f' :<->: g') = (f A.*** f') :<->: (g A.*** g')

left :: (a <-> b) -> (Either a c <-> Either b c)
left (f :<->: g) = A.left f :<->: A.left g

right :: (a <-> b) -> (Either c a <-> Either c b)
right (f :<->: g) = A.right f :<->: A.right g

infixr 2 +++
(+++) :: (a <-> b) -> (a' <-> b') -> (Either a a' <-> Either b b')
(f :<->: g) +++ (f' :<->: g') = (f A.+++ f') :<->: (g A.+++ g')

not :: Bool <-> Bool
not =
  [iso|
    False <-> True
    True <-> False
  |]

swap :: (a, b) <-> (b, a)
swap = [iso|(a, b) <-> (b, a)|]

constant :: a -> () <-> a
constant a = const a :<->: const ()

fst :: (a, ()) <-> a
fst = [iso|(a, ()) <-> a|]

snd :: ((), a) <-> a
snd = [iso|((), a) <-> a|]

isJust :: Maybe () <-> Bool
isJust =
  [iso|
    Just () <-> True
    Nothing <-> False
  |]

isNothing :: Maybe () <-> Bool
isNothing = not C.. isJust

switch :: Either a b <-> Either b a
switch =
  [iso|
    Left a <-> Right a
    Right a <-> Left a
  |]

isLeft :: Either () () <-> Bool
isLeft =
  [iso|
    Left () <-> True
    Right () <-> False
  |]

isRight :: Either () () <-> Bool
isRight = not C.. isLeft

lft :: Either a () <-> Maybe a
lft =
  [iso|
    Left a <-> Just a
    Right () <-> Nothing
  |]

rgt :: Either () a <-> Maybe a
rgt =
  [iso|
    Left () <-> Nothing
    Right a <-> Just a
  |]

cons :: (a, [a]) <-> [a]
cons = [iso|(a, l) <-> a:l|]

curried :: ((a, b) -> c) <-> (a -> b -> c)
curried = curry :<->: uncurry

defaultEq :: Eq a => a -> Maybe a <-> a
defaultEq d = fromMaybe d :<->: \a -> if a == d then Nothing else Just a
