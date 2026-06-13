{-# LANGUAGE BangPatterns #-}

-- | A persistent disjoint-set (union-find) structure for competitive programming.
--
-- It maintains a partition of 'Int' keys into disjoint sets and supports the
-- two classic near-O(1) operations: 'find' (which set does @x@ belong to?) and
-- 'union' (merge the sets containing @x@ and @y@). Both /union by rank/ and
-- /path compression/ are used, giving an effectively constant amortized cost
-- per operation (inverse-Ackermann).
--
-- Because the structure is built on a persistent 'IntMap', every operation
-- returns a /new/ 'DSU' and the old one stays valid. In particular 'find' and
-- 'connected' must thread the updated 'DSU' through, since they perform path
-- compression and therefore mutate the logical state.
--
-- == How to use
--
-- 1. Start from 'empty'. Keys are added lazily: any key mentioned by 'find',
--    'union', or 'connected' is auto-created as its own singleton set, so you
--    rarely need 'singleton' explicitly.
--
-- 2. Use 'union' to merge sets and 'connected' to test whether two keys share
--    a set. Remember to keep the returned 'DSU' for subsequent calls.
--
-- === Example: pure interface
--
-- @
-- demo :: Bool
-- demo =
--   let d0      = 'empty'
--       d1      = 'union' 1 2 d0
--       d2      = 'union' 2 3 d1
--       (c, _)  = 'connected' 1 3 d2   -- True: 1-2-3 are joined
--    in c
-- @
--
-- == State monad helpers
--
-- Threading the 'DSU' by hand is tedious, so the @*M@ variants ('findM',
-- 'unionM', 'connectedM') wrap the operations in a @'State' DSU@ monad
-- ('DSUM'). This lets you write a sequence of operations in @do@ notation.
--
-- === Example: filtering out redundant edges (Kruskal-style)
--
-- @
-- import Control.Monad.State.Strict (evalState)
--
-- -- Keep only the edges that connect two previously separate components.
-- spanningEdges :: [(Int, Int)] -> [(Int, Int)]
-- spanningEdges es = 'evalState' ('processEdges' es) 'empty'
-- @
--
-- == Notes
--
-- * Keys are arbitrary 'Int's; they do not need to be contiguous or start
--   from zero.
--
-- * 'find', 'connected', and 'processEdges' return an updated 'DSU' (directly
--   or via the state) because of path compression. Discarding it is correct
--   but forfeits the compression speedup.
--
-- * 'union' breaks ties by attaching the equal-rank root and bumping the new
--   root's rank by one, the standard union-by-rank rule.
module Precoded.EnhancedData.DisjointSet where

import Control.Monad (filterM)
import Control.Monad.State.Strict
import qualified Data.Graph as G
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')

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

-- | Build a 'DSU' whose sets are the (weakly) connected components of a
-- 'Data.Graph.Graph'. Every vertex of the graph becomes a key, and each edge
-- @(u, v)@ unions its endpoints; edge direction is ignored, so the result
-- groups vertices that are reachable when the graph is treated as undirected.
fromGraph :: G.Graph -> DSU
fromGraph g =
  let withVerts = foldr singleton empty (G.vertices g)
   in foldl' (\d (u, v) -> union u v d) withVerts (G.edges g)

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
