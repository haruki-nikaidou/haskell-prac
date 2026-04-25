module ABC455.A where

import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)

readInts :: IO [Int]
readInts = map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine

printAns :: Bool -> IO ()
printAns True = putStrLn "Yes"
printAns False = putStrLn "No"

main' :: IO ()
main' = do
  [a, b, c] <- readInts
  let ans = a /= b && b == c
  printAns ans
