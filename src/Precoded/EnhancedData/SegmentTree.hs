{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A FingerTree-backed segment tree for competitive programming.
--
-- It supports O(log n) range aggregation over a half-open interval @[l, r)@
-- and O(log n) point updates. The aggregation monoid is chosen per element
-- type through the 'Aggregable' class, so the same code works for range sum,
-- range min/max, range gcd, and so on.
--
-- == How to use
--
-- 1. Pick the value type you want to store and decide what to aggregate.
--
-- 2. Make it an instance of 'Aggregable' by choosing an aggregation monoid
--    ('Agg') and saying how a single element maps into it ('toAgg').
--
-- 3. 'build' the tree from a list, then 'rangeQuery' and 'update' as needed.
--
-- === Example: range sum + point update
--
-- @
-- import Data.Monoid (Sum (..))
--
-- newtype V = V Int
--
-- instance 'Aggregable' V where
--   type 'Agg' V = Sum Int
--   'toAgg' (V x) = Sum x
--
-- demo :: Int
-- demo =
--   let t  = 'build' [V 1, V 2, V 3, V 4, V 5]   -- [1,2,3,4,5]
--       s  = getSum ('rangeQuery' 1 4 t)          -- sum of [2,3,4] = 9
--       t' = 'update' 2 (V 10) t                  -- [1,2,10,4,5]
--       s' = getSum ('rangeQuery' 1 4 t')         -- sum of [2,10,4] = 16
--    in s + s'                                   -- 25
-- @
--
-- === Example: range minimum
--
-- @
-- import Data.Semigroup (Min (..))
--
-- newtype W = W Int
--
-- instance 'Aggregable' W where
--   -- Wrap in Maybe so the monoid has an identity for empty ranges.
--   type 'Agg' W = Maybe (Min Int)
--   'toAgg' (W x) = Just (Min x)
-- @
--
-- == Notes
--
-- * Indices are 0-based and ranges are half-open: 'rangeQuery' @l r@ covers
--   positions @l, l+1, .., r-1@. An empty range returns 'mempty' of 'Agg'.
--
-- * The 'Agg' type must be a 'Monoid'. If your operation only forms a
--   'Semigroup' (e.g. plain 'Min'/'Max'), wrap it in 'Maybe' to gain an
--   identity, as shown above.
--
-- * 'update' is a point update; out-of-range indices leave the tree
--   unchanged. The tree is persistent (immutable), so 'update' returns a
--   new tree and the old one stays valid.
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
