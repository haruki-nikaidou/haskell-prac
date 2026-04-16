module Read where

import Control.Monad (replicateM)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)

readInt :: IO Int
readInt = fst . fromJust . BS.readInt <$> BS.getLine

readInts :: IO [Int]
readInts = map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine

readQueries :: (BS.ByteString -> q) -> Maybe Int -> IO [q]
readQueries f (Just n) = map f <$> replicateM n BS.getLine
readQueries f Nothing = map f . BS.lines <$> BS.getContents

readMatrix :: Maybe Int -> Maybe Int -> IO [[Int]]
readMatrix rows cols = do
  rawLines <- case rows of
    Just n -> replicateM n BS.getLine
    Nothing -> BS.lines <$> BS.getContents
  return $ map (parseRow cols) rawLines
  where
    parseRow Nothing line = map (fst . fromJust . BS.readInt) (BS.words line)
    parseRow (Just c) line =
      let xs = map (fst . fromJust . BS.readInt) (BS.words line)
       in assert (length xs == c) xs -- optional: remove if perf-sensitive
    assert cond x = if cond then x else error "readMatrix: column count mismatch"

readString :: IO BS.ByteString
readString = BS.getLine
