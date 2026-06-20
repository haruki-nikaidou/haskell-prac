module ABC463.A where

import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)

readInts :: IO [Int]
readInts = map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine

solve :: Int -> Int -> Bool
solve w h = w * 9 == h * 16

printAns :: Bool -> IO ()
printAns True = putStrLn "Yes"
printAns False = putStrLn "No"

main :: IO ()
main = do
  [w, h] <- readInts
  let ans = solve w h
  printAns ans
