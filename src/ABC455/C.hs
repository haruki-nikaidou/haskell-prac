module ABC455.C where

import qualified Data.ByteString.Char8 as BS
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (sort)
import Data.Maybe (fromJust)

readInts :: IO [Int]
readInts = map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine

count :: [Int] -> IntMap Int
count xs = IntMap.fromListWith (+) [(x, 1) | x <- xs]

gain :: IntMap Int -> IntMap Int
gain = IntMap.mapWithKey (*)

solve :: Int -> IntMap Int -> Int
solve m = sum . dropEnd m . sort . IntMap.elems
  where
    dropEnd n xs = zipWith const xs (drop n xs)

main' :: IO ()
main' = do
  [_n, k] <- readInts
  serial <- gain <$> count <$> readInts
  let ans = solve k serial
   in print ans
