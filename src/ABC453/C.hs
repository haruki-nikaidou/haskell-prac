module ABC453.C where

import Read (readInt, readInts)

crossed :: Int -> Int -> Bool
crossed x y = x < 0 && y >= 0 || x >= 0 && y < 0

solve :: Int -> [Int] -> Int
solve _ [] = 0
solve pos (first : rest) =
  max left right
  where
    pos1 = pos + first
    pos2 = pos - first
    left = (if crossed pos pos1 then 1 else 0) + solve pos1 rest
    right = (if crossed pos pos2 then 1 else 0) + solve pos2 rest

main' :: IO ()
main' = do
  _ <- readInt
  ls <- readInts
  print $ solve 0 ls
