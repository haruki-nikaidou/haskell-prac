module C4.C8ARX where

import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)

readInts :: IO [Int]
readInts = map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine

allSubsequences :: [a] -> [[a]]
allSubsequences [] = [[]]
allSubsequences (x : xs) = let subs = allSubsequences xs in subs ++ map (x :) subs

solve :: [Int] -> Int -> Bool
solve list target = any (== target) (map sum (allSubsequences list))

printYesNo :: Bool -> IO ()
printYesNo True = putStrLn "Yes"
printYesNo False = putStrLn "No"

main :: IO ()
main = do
  [_, target] <- readInts
  list <- readInts
  printYesNo $ solve list target
