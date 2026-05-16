module ABC458.B where

import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)

readInts :: IO [Int]
readInts = map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine

solve :: Int -> Int -> [[Int]]
solve h w
  | h == 1 && w == 1 = [[0]]
  | h == 1 = [1 : replicate (w - 2) 2 ++ [1]]
  | w == 1 = [[1] ++ replicate (h - 2) 2 ++ [1]]
  | otherwise = grid h w

grid :: Int -> Int -> [[Int]]
grid rows cols =
  [ [cellAt r c | c <- [0 .. cols - 1]]
  | r <- [0 .. rows - 1]
  ]
  where
    cellAt r c
      | isCorner r c = 2
      | isEdge r c = 3
      | otherwise = 4

    isCorner r c = (r == 0 || r == rows - 1) && (c == 0 || c == cols - 1)
    isEdge r c = r == 0 || r == rows - 1 || c == 0 || c == cols - 1

printAns :: [[Int]] -> IO ()
printAns ans = mapM_ (putStrLn . unwords . map show) ans

main :: IO ()
main = do
  [h, w] <- readInts
  let ans = solve h w
  printAns ans
