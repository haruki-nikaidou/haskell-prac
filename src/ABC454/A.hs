module ABC454.A where

import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)

readInt :: IO Int
readInt = fst . fromJust . BS.readInt <$> BS.getLine

readInts :: IO [Int]
readInts = map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine

main' :: IO ()
main' = do
  [l, r] <- readInts
  let ans = r - l + 1
  print ans
