module ABC463.C where

import Control.Monad (replicateM)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

readInt :: IO Int
readInt = fst . fromJust . BS.readInt <$> BS.getLine

readInts :: IO [Int]
readInts = map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine

readQueries :: (BS.ByteString -> q) -> Maybe Int -> IO [q]
readQueries f (Just n) = map f <$> replicateM n BS.getLine
readQueries f Nothing = map f . BS.lines <$> BS.getContents

parseDualInt :: BS.ByteString -> (Int, Int)
parseDualInt line =
  let [int1_word, int2_word] = BS.words line
      int1_val = fst . fromJust $ BS.readInt int1_word
      int2_val = fst . fromJust $ BS.readInt int2_word
   in (int1_val, int2_val)

solve :: BS.ByteString -> Int
solve bs =
  sum
    [ min i (n - 1 - i) + 1
    | i <- [0 .. n - 1],
      BS.index bs i == 'C'
    ]
  where
    n = BS.length bs

printAns :: () -> IO ()
printAns ans = putStrLn (show ans)

main :: IO ()
main = do
  n <- readInt
  hl_pairs <- readQueries parseDualInt (Just n)
  _ <- readInt
  queries <- readInts
  let ans = ()
  printAns ans
