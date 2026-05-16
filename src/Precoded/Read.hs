module Precoded.Read where

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

readMatrix :: Maybe Int -> Maybe Int -> IO (V.Vector (VU.Vector Int))
readMatrix rows cols = do
  rawLines <- case rows of
    Just n -> replicateM n BS.getLine
    Nothing -> BS.lines <$> BS.getContents
  return $ V.fromList $ map (parseRow cols) rawLines
  where
    parseRow Nothing line = VU.fromList $ map (fst . fromJust . BS.readInt) (BS.words line)
    parseRow (Just c) line =
      let xs = VU.fromList $ map (fst . fromJust . BS.readInt) (BS.words line)
       in assert (VU.length xs == c) xs -- optional: remove if perf-sensitive
    assert cond x = if cond then x else error "readMatrix: column count mismatch"

readString :: IO BS.ByteString
readString = BS.getLine
