{-# LANGUAGE TypeOperators #-}
module Databrary.Iso.Types
  ( Invariant(..)
  , (<->)(..)
  , (<$>)
  ) where

import Prelude hiding ((<$>))
import qualified Control.Category as Cat

-- see also invariant package
class Invariant f where
  invMap :: (a -> b) -> (b -> a) -> f a -> f b

infix 2 <->, :<->:
data (<->) a b = (:<->:)
  { isoF :: a -> b
  , isoG :: b -> a
  }

instance Cat.Category (<->) where
  id = id :<->: id
  (f1 :<->: g1) . (f2 :<->: g2) = f1 . f2 :<->: g2 . g1

infixl 4 <$>
(<$>) :: Invariant f => (<->) a b -> f a -> f b
(<$>) (f :<->: g) = invMap f g
