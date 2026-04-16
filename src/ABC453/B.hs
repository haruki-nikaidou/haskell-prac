module ABC453.B where

import qualified Data.ByteString.Char8 as BS
import Data.List (mapAccumL)
import Data.Maybe (mapMaybe)
import Read (readInts)

solve :: Int -> [Int] -> [(Int, Int)]
solve _ [] = []
solve threshold raw_data =
  let indexed = zip [0 ..] raw_data
      start = head indexed
      filterRaw lastVal (idx, val)
        | filterData threshold lastVal (idx, val) = (val, Just (idx, val))
        | otherwise = (val, Nothing)
      (_, maybes) = mapAccumL filterRaw (snd start) (tail indexed)
   in start : mapMaybe id maybes

filterData :: Int -> Int -> (Int, Int) -> Bool
filterData threshold lastVal (_, val) = abs (val - lastVal) >= threshold

main' :: IO ()
main' = do
  [_, x] <- readInts
  vs <- readInts
  let ans = solve x vs
  mapM_
    ( \(idx, val) ->
        BS.putStrLn $
          BS.pack (show idx) <> BS.singleton ' ' <> BS.pack (show val)
    )
    ans
