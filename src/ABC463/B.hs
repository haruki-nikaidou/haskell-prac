module ABC463.B where

import Control.Monad (replicateM)
import qualified Data.ByteString.Char8 as BS
import Data.List (transpose)
import Data.Maybe (fromJust)

readIntChar :: IO (Int, Char)
readIntChar = do
  bs <- BS.getLine
  let [int_word, char_word] = BS.words bs
  let int_val = fst . fromJust $ BS.readInt int_word
  let char_val = BS.head char_word
  return (int_val, char_val)

parseChar :: Char -> Int
parseChar 'A' = 0
parseChar 'B' = 1
parseChar 'C' = 2
parseChar 'D' = 3
parseChar 'E' = 4

readQueries :: (BS.ByteString -> q) -> Maybe Int -> IO [q]
readQueries f (Just n) = map f <$> replicateM n BS.getLine
readQueries f Nothing = map f . BS.lines <$> BS.getContents

parseQuery :: BS.ByteString -> [Bool]
parseQuery line = map parseChar (BS.unpack line)
  where
    parseChar 'o' = True
    parseChar 'x' = False

foldOrBoolMatrix :: [[Bool]] -> [Bool]
foldOrBoolMatrix = map or . transpose

solve :: Int -> [[Bool]] -> Bool
solve query_on bool_vectors =
  let folded = foldOrBoolMatrix bool_vectors
   in folded !! query_on

printAns :: Bool -> IO ()
printAns True = putStrLn "Yes"
printAns False = putStrLn "No"

main :: IO ()
main = do
  (_, query_char) <- readIntChar
  let query_on = parseChar query_char
  bool_vectors <- readQueries parseQuery Nothing
  let ans = solve query_on bool_vectors
  printAns ans
