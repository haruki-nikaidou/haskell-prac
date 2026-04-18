module ABC454.B where

import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import qualified Data.Set as Set

readInts :: IO [Int]
readInts = map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine

solve :: [Int] -> Int -> (Bool, Bool)
solve wearing clothes =
  let unique_clothes = Set.fromList wearing
   in (Set.size unique_clothes == length wearing, Set.size unique_clothes == clothes)

printAns :: (Bool, Bool) -> IO ()
printAns (True, True) = do
  putStrLn "Yes"
  putStrLn "Yes"
printAns (True, False) = do
  putStrLn "Yes"
  putStrLn "No"
printAns (False, True) = do
  putStrLn "No"
  putStrLn "Yes"
printAns (False, False) = do
  putStrLn "No"
  putStrLn "No"

main' :: IO ()
main' = do
  [n, m] <- readInts
  clothes <- readInts
  printAns $ solve clothes m
