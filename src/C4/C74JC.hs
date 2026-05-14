module C4.C74JC where

import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import qualified Data.Sequence as Seq

readInts :: IO [Int]
readInts = map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine

main :: IO ()
main = do
  [_, k] <- readInts
  list <- readInts
  let ans = Seq.index (Seq.sort $ Seq.fromList list) (k - 1)
  print ans
