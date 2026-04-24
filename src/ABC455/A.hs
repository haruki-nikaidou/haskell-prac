module ABC455.A where

import Read (readInts)

main' :: IO ()
main' = do
  n <- readInts
  print $ sum n
