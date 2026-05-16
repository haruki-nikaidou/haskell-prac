module ABC458.D where

import Control.Monad (forM, replicateM)
import Control.Monad.State (State, evalState, get, gets, put)
import qualified Data.ByteString.Char8 as BS
import Data.Heap (MaxHeap, MinHeap)
import qualified Data.Heap as H
import Data.Maybe (fromJust)

readInt :: IO Int
readInt = fst . fromJust . BS.readInt <$> BS.getLine

readInts :: IO [Int]
readInts = map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine

readQueries :: (BS.ByteString -> q) -> Maybe Int -> IO [q]
readQueries f (Just n) = map f <$> replicateM n BS.getLine
readQueries f Nothing = map f . BS.lines <$> BS.getContents

parseQuery :: BS.ByteString -> [Int]
parseQuery line =
  let [a, b] = map (fst . fromJust . BS.readInt) (BS.words line)
   in [a, b]

type MedianState = (MaxHeap Int, MinHeap Int)

insert :: Int -> State MedianState ()
insert x = do
  (lo, hi) <- get
  -- Route x to the correct half.
  let (lo', hi') = case H.viewHead lo of
        Just top | x <= top -> (H.insert x lo, hi)
        _ -> (lo, H.insert x hi)
  put (rebalance lo' hi')

rebalance :: MaxHeap Int -> MinHeap Int -> MedianState
rebalance lo hi
  | H.size lo > H.size hi + 1 =
      let Just (m, lo') = H.view lo
       in rebalance lo' (H.insert m hi)
  | H.size hi > H.size lo =
      let Just (m, hi') = H.view hi
       in rebalance (H.insert m lo) hi'
  | otherwise = (lo, hi)

median :: State MedianState Int
median = do
  lo <- gets fst
  case H.viewHead lo of
    Just m -> pure m

step :: [Int] -> State MedianState Int
step [a, b] = insert a >> insert b >> median

solve :: Int -> [[Int]] -> [Int]
solve first queries = evalState (forM queries step) (H.singleton first, H.empty)

printAns :: [Int] -> IO ()
printAns ans = putStrLn (unlines (map show ans))

main :: IO ()
main = do
  first <- readInt
  queries_amount <- readInt
  queries <- readQueries parseQuery (Just queries_amount)
  let ans = solve first (queries)
   in printAns ans
