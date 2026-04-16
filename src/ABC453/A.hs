module ABC001.A where

import qualified Data.ByteString.Char8 as BS
import Lib (readInt, readString)

solve :: IO ()
solve = do
  lead <- readInt
  a <- readString
  BS.putStrLn $ dropLeadingOs lead a

dropLeadingOs :: Int -> BS.ByteString -> BS.ByteString
dropLeadingOs lead xs = go lead xs
  where
    go _ bs | BS.null bs = BS.empty
    go r bs | r <= 0 = bs -- counter exhausted, keep all
    go _ bs | BS.head bs /= 'o' = bs -- non-'o' found, keep all
    go r bs = go (r - 1) (BS.tail bs) -- it's an 'o', drop it
