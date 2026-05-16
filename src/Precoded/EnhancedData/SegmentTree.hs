{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Precoded.EnhancedData.SegmentTree where

import Data.FingerTree (FingerTree, Measured (..), (><))
import qualified Data.FingerTree as FT
import Data.Monoid (Sum (..))

data Sized m = Sized {sizeOf :: !(Sum Int), aggOf :: !m}

instance (Semigroup m) => Semigroup (Sized m) where
  Sized s1 a1 <> Sized s2 a2 = Sized (s1 <> s2) (a1 <> a2)

instance (Monoid m) => Monoid (Sized m) where
  mempty = Sized mempty mempty

class (Monoid (Agg a)) => Aggregable a where
  type Agg a
  toAgg :: a -> Agg a

newtype SegElem a = Elem {getElem :: a}

instance (Aggregable a, m ~ Agg a) => Measured (Sized m) (SegElem a) where
  measure (Elem x) = Sized (Sum 1) (toAgg x)

type SegTree a = FingerTree (Sized (Agg a)) (SegElem a)

build :: (Aggregable a) => [a] -> SegTree a
build = FT.fromList . map Elem

-- Aggregate over [l, r), O(log n).
rangeQuery :: (Aggregable a) => Int -> Int -> SegTree a -> Agg a
rangeQuery l r t =
  let n = r - l
      (_, rest) = FT.split (\m -> getSum (sizeOf m) > l) t
      (mid, _) = FT.split (\m -> getSum (sizeOf m) >= n) rest
   in aggOf (measure mid)

-- Point update at index i, O(log n).
update :: (Aggregable a) => Int -> a -> SegTree a -> SegTree a
update i x t =
  let (left, rest) = FT.split (\m -> getSum (sizeOf m) > i) t
   in case FT.viewl rest of
        FT.EmptyL -> t
        _ FT.:< rest' -> (left FT.|> Elem x) >< rest'
