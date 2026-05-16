module Precoded.EnhancedData.ChthollyTree where

-- idea comes from https://codeforces.com/problemset/problem/896/C

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

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

newtype ChthollyTree a = ChthollyTree {getTree :: Map Int (RangeNode a)}
  deriving (Show)

empty :: ChthollyTree a
empty = ChthollyTree Map.empty

fromList :: (Eq a) => [(Int, a)] -> ChthollyTree a
fromList = foldr step empty . runs
  where
    runs [] = []
    runs ((i, v) : xs) =
      let (same, rest) = span (\(j, w) -> w == v && j == i + length same) xs
       in (i, i + 1 + length same, v) : runs rest
    step (l, r, v) (ChthollyTree m) =
      ChthollyTree (Map.insert l (RangeNode l r v) m)

insertNode :: RangeNode a -> ChthollyTree a -> ChthollyTree a
insertNode n (ChthollyTree m) = ChthollyTree (Map.insert (left n) n m)

splitAt' :: Int -> ChthollyTree a -> ChthollyTree a
splitAt' i t@(ChthollyTree m) =
  case Map.lookupLE i m of
    Just (_, (RangeNode l r v))
      | i > l && i < r ->
          let leftPart = RangeNode l i v
              rightPart = RangeNode i r v
           in ChthollyTree (Map.insert i rightPart (Map.insert l leftPart m))
      | otherwise -> t
    Nothing -> t

assign :: (Eq a) => Int -> Int -> a -> ChthollyTree a -> ChthollyTree a
assign l r v = updateRange l r (const v)

coalesceAt :: (Eq a) => Int -> ChthollyTree a -> ChthollyTree a
coalesceAt k t@(ChthollyTree m) =
  case Map.lookup k m of
    Just (RangeNode l r v)
      | Just (_, RangeNode l' r' v') <- Map.lookupLT l m,
        r' == l,
        v' == v ->
          ChthollyTree
            . Map.insert l' (RangeNode l' r v)
            . Map.delete l
            $ m
    _ -> t

lookup :: Int -> ChthollyTree a -> Maybe a
lookup i (ChthollyTree m) =
  case Map.lookupLE i m of
    Just (_, RangeNode _ r v) | i < r -> Just v
    _ -> Nothing

toList :: ChthollyTree a -> [(Int, a)]
toList (ChthollyTree m) =
  concatMap (\(RangeNode l r v) -> [(i, v) | i <- [l .. r - 1]]) (Map.elems m)

instance Foldable ChthollyTree where
  foldr f z (ChthollyTree m) =
    Map.foldr (\(RangeNode l r v) acc -> foldrTimes (r - l) f v acc) z m
    where
      foldrTimes 0 _ _ acc = acc
      foldrTimes n g x acc = g x (foldrTimes (n - 1) g x acc)

  length (ChthollyTree m) =
    Map.foldr (\(RangeNode l r _) acc -> acc + (r - l)) 0 m

updateRange :: (Eq a) => Int -> Int -> (a -> a) -> ChthollyTree a -> ChthollyTree a
updateRange a b f tree
  | a >= b = tree
  | otherwise =
      let ChthollyTree m0 = splitAt' b (splitAt' a tree)
          (below, _, fromA) = Map.splitLookup a m0
          (inside, mB, above) = Map.splitLookup b fromA
          above' = maybe above (\n -> Map.insert b n above) mB
          inside' = Map.map (\(RangeNode l r v) -> RangeNode l r (f v)) inside
          m1 = Map.unions [below, inside', above']
          coalesceInside :: (Eq a) => Int -> Int -> ChthollyTree a -> ChthollyTree a
          coalesceInside a' b' (ChthollyTree m) =
            let -- Collect keys in [a, b] in ascending order; coalesce each in turn.
                keysToCheck = takeWhile (<= b') . dropWhile (< a') $ Map.keys m
             in foldr coalesceAt (ChthollyTree m) keysToCheck
       in coalesceInside a b (ChthollyTree m1)
