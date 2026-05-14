module C4.C2YQS where

import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)

readInts :: IO [Int]
readInts = map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine

solve :: Int -> Int -> Int
solve a b
  | a >= b = 10 * a + b
  | otherwise = 10 * b + a

main :: IO ()
main = do
  [a, b] <- readInts
  print $ solve a b
