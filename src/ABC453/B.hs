module ABC453.B where

-- WA: https://atcoder.jp/contests/abc453/submissions/74997070
-- CE: https://atcoder.jp/contests/abc453/submissions/74997214
-- AC: https://atcoder.jp/contests/abc453/submissions/74997223
import Data.List (mapAccumL)
import Data.Maybe (mapMaybe)
import Read (readInts)

solve :: Int -> [Int] -> [(Int, Int)]
solve _ [] = []
solve threshold raw_data =
  let indexed = zip [0 ..] raw_data
      start = head indexed
      filterRaw = filterData threshold
      (_, maybes) = mapAccumL filterRaw (snd start) (tail indexed)
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
