module ABC458.G where

import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)

readInts :: IO [Int]
readInts = map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine

solve :: Int -> Int -> Int
solve a b = a + b

printAns :: Int -> IO ()
printAns ans = putStrLn (show ans)

main :: IO ()
main = do
  [a, b] <- readInts
  let ans = solve a b
  printAns ans
