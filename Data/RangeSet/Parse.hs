{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.RangeSet.Parse
  ( parseRangeSets
  ) where

import Control.Applicative ((<$>), optional)
import Control.Monad (liftM2)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import qualified Data.RangeSet.List as R
import qualified Text.ParserCombinators.ReadP as RP
import qualified Text.ParserCombinators.ReadPrec as RP (lift, readPrec_to_P, minPrec)
import Text.Read (readMaybe, readPrec)

newtype RangeList a = RangeList [(a,a)] deriving (Monoid)

readP :: Read a => RP.ReadP a
readP = RP.readPrec_to_P readPrec RP.minPrec

instance (Read a, Bounded a) => Read (RangeList a) where
  readPrec = RP.lift $ RangeList <$> RP.sepBy rr (RP.char ',') where
    ru = do
      _ <- RP.char '-'
      RP.option maxBound readP
    rr = do
      l <- optional readP
      let lb = fromMaybe minBound l
      ub <- maybe ru (`RP.option` ru) l
      return (lb, ub)

rangeListSet :: (Ord a, Enum a) => RangeList a -> R.RSet a
rangeListSet (RangeList l) = R.fromRangeList l

parseRangeSets :: (Ord a, Enum a, Bounded a, Read a) => [String] -> [String] -> Maybe (R.RSet a)
parseRangeSets incl excl = liftM2 R.difference (if null incl then Just R.full else prl incl) (prl excl) where
  prl = fmap (rangeListSet . mconcat) . mapM readMaybe
