module ABC458.C where

import qualified Data.ByteString.Char8 as BS

solve :: BS.ByteString -> Int
solve bs =
  sum
    [ min i (n - 1 - i) + 1
    | i <- [0 .. n - 1],
      BS.index bs i == 'C'
    ]
  where
    n = BS.length bs

printAns :: Int -> IO ()
printAns ans = putStrLn (show ans)

readString :: IO BS.ByteString
readString = BS.getLine

main :: IO ()
main = do
  s <- readString
  let ans = solve s
  printAns ans
