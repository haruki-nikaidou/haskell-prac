module ABC455.D where

import Control.Monad (replicateM)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq

readInts :: IO [Int]
readInts = map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine

readQueries :: (BS.ByteString -> q) -> Maybe Int -> IO [q]
readQueries f (Just n) = map f <$> replicateM n BS.getLine
readQueries f Nothing = map f . BS.lines <$> BS.getContents

moveAbove :: [Seq Int] -> (Int, Int) -> [Seq Int]
moveAbove stacks (p, q) =
  let splitAtP s = case Seq.findIndexL (== p) s of
        Just i -> Just (Seq.splitAt i s)
        Nothing -> Nothing

      hasQ s = p == q || q `elem` s

      tagged = [(s, splitAtP s, hasQ s) | s <- stacks]

      chunk = case [fp | (_, Just (_, fp), _) <- tagged] of
        (c : _) -> c
        [] -> Seq.empty
   in [ case (mSplit, containsQ) of
          (Just (below, _), False) -> below -- source stack: keep below p
          (_, True) -> s >< chunk -- dest stack: append chunk
          _ -> s -- untouched: share original
      | (s, mSplit, containsQ) <- tagged
      ]

initializeStacks :: Int -> [Seq Int]
initializeStacks n = [Seq.singleton i | i <- [1 .. n]]

solve :: Int -> [(Int, Int)] -> [Seq Int]
solve n operations = foldl moveAbove (initializeStacks n) operations

readOperation :: BS.ByteString -> (Int, Int)
readOperation line =
  let [a, b] = map (fst . fromJust . BS.readInt) (BS.words line)
   in (a, b)

printAns :: [Seq Int] -> IO ()
printAns stacks = putStrLn $ unwords $ map (show . Seq.length) stacks

main' :: IO ()
main' = do
  [n, q] <- readInts
  operations <- readQueries readOperation (Just q)
  let ans = solve n operations
   in printAns ans
