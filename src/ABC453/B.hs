module ABC453.B (main') where

-- WA: https://atcoder.jp/contests/abc453/submissions/74997070
-- CE: https://atcoder.jp/contests/abc453/submissions/74997214
-- AC: https://atcoder.jp/contests/abc453/submissions/74997223
import Data.List (mapAccumL)
import Data.Maybe (mapMaybe)
import Precoded.Read (readInts)

solve :: Int -> [Int] -> [(Int, Int)]
solve _ [] = []
solve threshold (v : vs) =
  let start = (0, v)
      indexed = zip [1 ..] vs
      filterRaw = filterData threshold
      (_, maybes) = mapAccumL filterRaw (snd start) indexed
   in start : mapMaybe id maybes

filterData :: Int -> Int -> (Int, Int) -> (Int, Maybe (Int, Int))
filterData threshold lastVal (idx, val)
  | abs (val - lastVal) >= threshold = (val, Just (idx, val))
  | otherwise = (lastVal, Nothing)

printAns :: [(Int, Int)] -> IO ()
printAns ans =
  let rows = map (\(idx, val) -> (show idx) <> " " <> (show val)) ans
   in mapM_ putStrLn rows

main' :: IO ()
main' = do
  [_, x] <- readInts
  vs <- readInts
  let ans = solve x vs
  printAns ans
