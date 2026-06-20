{-# LANGUAGE LambdaCase #-}

module Precoded.Algorithm.DP where

import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M

data Node a = Node {cache :: IORef (Maybe a), compute :: IO a}

type Vertex = Int

force :: Node a -> IO a
force n =
  readIORef (cache n) >>= \case
    Just v -> pure v
    Nothing -> do
      v <- compute n
      writeIORef (cache n) (Just v)
      pure v

mkGraph ::
  (Vertex -> [Vertex]) ->
  (Vertex -> [a] -> a) ->
  IO (Vertex -> IO (Node a))
mkGraph depsOf combine = do
  reg <- newIORef (M.empty :: Map Vertex (Node a))
  let getNode v =
        readIORef reg >>= \m -> case M.lookup v m of
          Just n -> pure n
          Nothing -> do
            c <- newIORef Nothing
            let n = Node c $ do
                  ds <- mapM getNode (depsOf v)
                  vs <- mapM force ds
                  pure (combine v vs)
            modifyIORef' reg (M.insert v n)
            pure n
  pure getNode

data KnapsackItem = KnapsackItem {weight :: Int, value :: Int}

knapsack :: Int -> [KnapsackItem] -> IO Int
knapsack capacity items = do
  getNode <- mkGraph depsOf combine
  getNode (enc n capacity) >>= force
  where
    n = length items
    -- 1-indexed map of items so vertex i corresponds to itemMap ! i
    itemMap = M.fromList (zip [1 ..] items)

    -- Encode the table cell (i, w) as a single vertex.
    enc i w = i * (capacity + 1) + w
    rowOf v = v `div` (capacity + 1) -- i: number of items considered
    colOf v = v `mod` (capacity + 1) -- w: remaining capacity

    depsOf v
      | i == 0 = []
      | weight item <= w = [enc (i - 1) w, enc (i - 1) (w - weight item)]
      | otherwise = [enc (i - 1) w]
      where
        i = rowOf v
        w = colOf v
        item = itemMap M.! i

    -- vs lines up with depsOf: [skip] or [skip, take].
    combine v vs = case (rowOf v, vs) of
      (0, _) -> 0
      (i, [skip]) -> skip
      (i, [skip, take']) -> max skip (value (itemMap M.! i) + take')
      _ -> 0
