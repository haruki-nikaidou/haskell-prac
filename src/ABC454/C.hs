module ABC454.C where

import Control.Monad (replicateM)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Graph as G
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set

readQueries :: (BS.ByteString -> q) -> Maybe Int -> IO [q]
readQueries f (Just n) = map f <$> replicateM n BS.getLine
readQueries f Nothing = map f . BS.lines <$> BS.getContents

readEdge :: BS.ByteString -> (Int, Int)
readEdge line =
  let [a, b] = map (fst . fromJust . BS.readInt) (BS.words line)
   in (a, b)

countReachable :: Int -> [(Int, Int)] -> Int
countReachable start edges =
  length (G.reachable g startVertex)
  where
    adj = Map.toList $ foldr (\(u, v) -> Map.insertWith (++) u [v]) Map.empty edges
    allNodes = Set.toList . Set.fromList $ concatMap (\(u, v) -> [u, v]) edges
    triples = [(n, n, Map.findWithDefault [] n (Map.fromList adj)) | n <- allNodes]
    (g, _, keyToVertex) = G.graphFromEdges triples
    startVertex = fromJust (keyToVertex start)

main' :: IO ()
main' = do
  [n, m] <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  edges <- readQueries readEdge (Just m)
  let ans = countReachable 1 edges
  print ans
