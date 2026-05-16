{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Precoded.EnhancedData.SegmentTree
  ( Measured (..),
    SegTree (..),
    summary,
    build,
    (!!),
    insert,
    delete,
    fork,
  )
where

import Data.Foldable (toList)
import Data.List (sort)
import Prelude hiding ((!!))

class (Monoid b) => Measured a b where
  measure :: a -> b

data SegTree a b = Empty | Leaf a b | Node (a, a) b (SegTree a b) (SegTree a b)

summary :: (Measured a b) => SegTree a b -> b
summary Empty = mempty
summary (Leaf _ m) = m
summary (Node _ m _ _) = m

build :: (Measured a b, Ord a, Foldable t) => t a -> SegTree a b
build = go . sort . toList
  where
    go [] = Empty
    go [x] = Leaf x (measure x)
    go xs =
      let n = length xs
          (lo : _) = xs
          (ls, rs) = splitAt (n `div` 2) xs
          hi = last rs
          left = go ls
          right = go rs
       in Node (lo, hi) (summary left <> summary right) left right

(!!) :: (Measured a b, Ord a) => SegTree a b -> (a, a) -> b
(!!) Empty _ = mempty
(!!) (Leaf k m) (lo, hi)
  | lo <= k && k <= hi = m
  | otherwise = mempty
(!!) (Node (nlo, nhi) m left right) (lo, hi)
  | hi < nlo || lo > nhi = mempty
  | lo <= nlo && nhi <= hi = m
  | otherwise = (left !! (lo, hi)) <> (right !! (lo, hi))

-- ---------------------------------------------------------------------------
-- Helpers for AVL-style balancing
-- ---------------------------------------------------------------------------

-- | Minimum key in a non-empty tree.
minKey :: SegTree a b -> a
minKey (Leaf k _) = k
minKey (Node (lo, _) _ _ _) = lo
minKey Empty = error "minKey: empty tree"

-- | Maximum key in a non-empty tree.
maxKey :: SegTree a b -> a
maxKey (Leaf k _) = k
maxKey (Node (_, hi) _ _ _) = hi
maxKey Empty = error "maxKey: empty tree"

-- | Height of a tree.
--   NOTE: O(n). Storing height in each node would give O(1), but that
--   requires changing the data type.
height :: SegTree a b -> Int
height Empty = 0
height (Leaf _ _) = 1
height (Node _ _ l r) = 1 + max (height l) (height r)

-- | Balance factor (height left − height right).
balanceFactor :: SegTree a b -> Int
balanceFactor (Node _ _ l r) = height l - height r
balanceFactor _ = 0

-- | Smart constructor: builds an internal Node from two non-empty
--   children, computing the key range and monoidal summary automatically.
mkNode :: (Measured a b) => SegTree a b -> SegTree a b -> SegTree a b
mkNode l r = Node (minKey l, maxKey r) (summary l <> summary r) l r

-- | Right rotation (the left child becomes the new root).
rotateR :: (Measured a b) => SegTree a b -> SegTree a b
rotateR (Node _ _ (Node _ _ ll lr) r) = mkNode ll (mkNode lr r)
rotateR t = t

-- | Left rotation (the right child becomes the new root).
rotateL :: (Measured a b) => SegTree a b -> SegTree a b
rotateL (Node _ _ l (Node _ _ rl rr)) = mkNode (mkNode l rl) rr
rotateL t = t

-- | AVL-style rebalancing of a single node.
balance :: (Measured a b) => SegTree a b -> SegTree a b
balance t@(Node _ _ l r)
  -- Left-heavy
  | bf > 1 && balanceFactor l >= 0 = rotateR t
  | bf > 1 = rotateR (mkNode (rotateL l) r)
  -- Right-heavy
  | bf < -1 && balanceFactor r <= 0 = rotateL t
  | bf < -1 = rotateL (mkNode l (rotateR r))
  | otherwise = t
  where
    bf = balanceFactor t
balance t = t

-- ---------------------------------------------------------------------------
-- insert / delete / fork
-- ---------------------------------------------------------------------------

-- | Insert an element into the segment tree, creating a new path from the
--   root to the affected leaf.  Duplicate keys update the existing leaf's
--   measure.  The tree is kept balanced via AVL rotations.
insert :: (Measured a b, Ord a) => a -> SegTree a b -> SegTree a b
insert x Empty = Leaf x (measure x)
insert x (Leaf k _)
  | x == k = Leaf k (measure x) -- update existing
  | x < k = mkNode (Leaf x (measure x)) (Leaf k (measure k))
  | otherwise = mkNode (Leaf k (measure k)) (Leaf x (measure x))
insert x (Node _ _ l r)
  | x <= maxKey l = balance (mkNode (insert x l) r)
  | otherwise = balance (mkNode l (insert x r))

-- | Delete the first occurrence of a key from the segment tree, creating a
--   new path from the root.  If the key is not present the tree is returned
--   unchanged.  The tree is kept balanced via AVL rotations.
delete :: (Measured a b, Ord a) => a -> SegTree a b -> SegTree a b
delete _ Empty = Empty
delete x (Leaf k m)
  | x == k = Empty
  | otherwise = Leaf k m
delete x (Node _ _ l r)
  | x <= maxKey l = case delete x l of
      Empty -> r
      l' -> balance (mkNode l' r)
  | otherwise = case delete x r of
      Empty -> l
      r' -> balance (mkNode l r')

-- | Split a tree at the root into its two immediate subtrees.
--   Returns (left, right) for a Node, (tree, Empty) for a Leaf,
--   and (Empty, Empty) for Empty.
fork :: SegTree a b -> (SegTree a b, SegTree a b)
fork (Node _ _ l r) = (l, r)
fork t = (t, Empty)
