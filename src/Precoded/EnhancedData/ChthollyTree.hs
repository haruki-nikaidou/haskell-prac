module Precoded.EnhancedData.ChthollyTree where

-- idea comes from https://codeforces.com/problemset/problem/896/C

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

data RangeNode a = RangeNode
  { left :: Int,
    right :: Int,
    value :: a
  }
  deriving (Show)

splitN :: Int -> RangeNode a -> Maybe (RangeNode a, RangeNode a)
splitN i (RangeNode l r v)
  | i <= l || i >= r = Nothing
  | otherwise = Just (RangeNode l i v, RangeNode i r v)

assignN :: (Eq a) => (Int, Int) -> a -> RangeNode a -> [RangeNode a]
assignN (l, r) v node@(RangeNode nl nr nv)
  | nv == v = [node]
  | r <= nl || l >= nr = [node]
  | l <= nl && r < nr = [RangeNode nl r v, RangeNode r nr nv]
  | l > nl && r >= nr = [RangeNode nl l nv, RangeNode l nr v]
  | otherwise = [RangeNode nl l nv, RangeNode l r v, RangeNode r nr nv]

toListN :: RangeNode a -> [(Int, a)]
toListN (RangeNode l r v) = [(i, v) | i <- [l .. r - 1]]

newtype RangeTree a = RangeTree {getTree :: Map Int (RangeNode a)}
  deriving (Show)

empty :: RangeTree a
empty = RangeTree Map.empty

fromList :: (Eq a) => [(Int, a)] -> RangeTree a
fromList = foldr step empty . runs
  where
    runs [] = []
    runs ((i, v) : xs) =
      let (same, rest) = span (\(j, w) -> w == v && j == i + length same) xs
       in (i, i + 1 + length same, v) : runs rest
    step (l, r, v) (RangeTree m) =
      RangeTree (Map.insert l (RangeNode l r v) m)

insertNode :: RangeNode a -> RangeTree a -> RangeTree a
insertNode n (RangeTree m) = RangeTree (Map.insert (left n) n m)

splitAt' :: Int -> RangeTree a -> RangeTree a
splitAt' i t@(RangeTree m) =
  case Map.lookupLE i m of
    Just (_, (RangeNode l r v))
      | i > l && i < r ->
          let leftPart = RangeNode l i v
              rightPart = RangeNode i r v
           in RangeTree (Map.insert i rightPart (Map.insert l leftPart m))
      | otherwise -> t
    Nothing -> t

assign :: (Eq a) => Int -> Int -> a -> RangeTree a -> RangeTree a
assign l r v tree
  | l >= r = tree
  | otherwise =
      let RangeTree m0 = splitAt' r (splitAt' l tree)
          (below, _, fromL) = Map.splitLookup l m0 -- node at key l (if any) is inside [l,r), drop it
          (_, mR, above) = Map.splitLookup r fromL -- node at key r stays (it's outside [l,r))
          above' = maybe above (\n -> Map.insert r n above) mR
          m1 = Map.insert l (RangeNode l r v) (Map.union below above')
       in coalesceAt r (coalesceAt l (RangeTree m1))

coalesceAt :: (Eq a) => Int -> RangeTree a -> RangeTree a
coalesceAt k t@(RangeTree m) =
  case Map.lookup k m of
    Just (RangeNode l r v)
      | Just (RangeNode l' r' v') <- Map.lookupLT l m,
        r' == l,
        v' == v ->
          RangeTree
            . Map.insert l' (RangeNode l' r v)
            . Map.delete l
            $ m
    _ -> t

lookup :: Int -> RangeTree a -> Maybe a
lookup i (RangeTree m) =
  case Map.lookupLE i m of
    Just (_, RangeNode _ r v) | i < r -> Just v
    _ -> Nothing

toList :: RangeTree a -> [(Int, a)]
toList (RangeTree m) =
  concatMap (\(RangeNode l r v) -> [(i, v) | i <- [l .. r - 1]]) (Map.elems m)
