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
