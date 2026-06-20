module ABC458.A where

import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)

readInt :: IO Int
readInt = fst . fromJust . BS.readInt <$> BS.getLine

readInts :: IO [Int]
readInts = map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine

readString :: IO BS.ByteString
readString = BS.getLine

slice :: Int -> Int -> BS.ByteString -> BS.ByteString
slice a b bs = BS.take (b - a) (BS.drop a bs)

solve :: BS.ByteString -> Int -> BS.ByteString
solve s remove_count = slice remove_count (BS.length s - remove_count) s

printAns :: Int -> IO ()
printAns ans = putStrLn (show ans)

main :: IO ()
main = do
  s <- readString
  remove_count <- readInt
  let ans = solve s remove_count
  BS.putStrLn ans
