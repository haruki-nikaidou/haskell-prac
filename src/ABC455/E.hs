module ABC455.E where

import Read (readInts)

solve :: () -> ()
solve () = undefined

main' :: IO ()
main' = do
  [a, b, c] <- readInts
  let ans = if a == b && b == c then "Yes" else "No"
  putStrLn ans
