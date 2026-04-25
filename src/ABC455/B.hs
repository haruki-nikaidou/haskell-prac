module ABC455.B where

import Control.Monad (replicateM)
import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)

readInts :: IO [Int]
readInts = map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine

readQueries :: (BS.ByteString -> q) -> Maybe Int -> IO [q]
readQueries f (Just n) = map f <$> replicateM n BS.getLine
readQueries f Nothing = map f . BS.lines <$> BS.getContents

data Color = Black | White deriving (Eq)

readCell :: Char -> Color
readCell '.' = White
readCell '#' = Black
readCell _ = error "Invalid cell character"

countPointSymmetric :: (Eq a) => [[a]] -> Int
countPointSymmetric grid =
  length
    [ ()
    | r1 <- [0 .. h - 1],
      r2 <- [r1 .. h - 1],
      c1 <- [0 .. w - 1],
      c2 <- [c1 .. w - 1],
      isPointSym r1 c1 r2 c2
    ]
  where
    h = length grid
    w = if h == 0 then 0 else length (head grid)
    arr = listArray ((0, 0), (h - 1, w - 1)) (concat grid)
    isPointSym r1 c1 r2 c2 =
      and
        [ arr ! (r1 + i, c1 + j) == arr ! (r2 - i, c2 - j)
        | i <- [0 .. (r2 - r1)],
          j <- [0 .. (c2 - c1)],
          (i, j) <= (r2 - r1 - i, c2 - c1 - j) -- only check half
        ]

solve :: [[Color]] -> Int
solve grid = countPointSymmetric grid

main' :: IO ()
main' = do
  [h, _w] <- readInts
  grid <- readQueries (map readCell . BS.unpack) (Just h)
  let ans = solve grid
   in putStrLn (show ans)
