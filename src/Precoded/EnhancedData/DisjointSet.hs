{-# LANGUAGE BangPatterns #-}

module Precoded.EnhancedData.DisjointSet where

import Control.Monad (filterM)
import Control.Monad.State.Strict
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

data DSU = DSU
  { parent :: !(IntMap Int),
    rank :: !(IntMap Int)
  }
  deriving (Show)

empty :: DSU
empty = DSU IM.empty IM.empty

singleton :: Int -> DSU -> DSU
singleton x d@(DSU p r)
  | IM.member x p = d
  | otherwise = DSU (IM.insert x x p) (IM.insert x 0 r)

find :: Int -> DSU -> (Int, DSU)
find x d0 =
  let d = singleton x d0
      (root, compressed) = go x d
   in (root, compressed)
  where
    go i d@(DSU p _) =
      let p_i = p IM.! i
       in if p_i == i
            then (i, d)
            else
              let (root, DSU p' r') = go p_i d
                  p'' = IM.insert i root p'
               in (root, DSU p'' r')

connected :: Int -> Int -> DSU -> (Bool, DSU)
connected x y d0 =
  let (rx, d1) = find x d0
      (ry, d2) = find y d1
   in (rx == ry, d2)

union :: Int -> Int -> DSU -> DSU
union x y d0 =
  let (rx, d1) = find x d0
      (ry, d2) = find y d1
   in if rx == ry
        then d2
        else
          let DSU p r = d2
              !rrx = r IM.! rx
              !rry = r IM.! ry
           in case compare rrx rry of
                LT -> DSU (IM.insert rx ry p) r
                GT -> DSU (IM.insert ry rx p) r
                EQ -> DSU (IM.insert ry rx p) (IM.insert rx (rrx + 1) r)

type DSUM = State DSU

findM :: Int -> DSUM Int
findM x = state (find x)

unionM :: Int -> Int -> DSUM ()
unionM x y = modify' (union x y)

connectedM :: Int -> Int -> DSUM Bool
connectedM x y = state (connected x y)

processEdges :: [(Int, Int)] -> DSUM [(Int, Int)]
processEdges = filterM $ \(u, v) -> do
  c <- connectedM u v
  if c
    then pure False
    else unionM u v >> pure True
