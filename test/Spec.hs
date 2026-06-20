module Main (main) where

import Control.Monad (forM)
import Data.IORef
import Precoded.Algorithm.DP (KnapsackItem (..), knapsack)
import System.Exit (exitFailure)

-- | A single knapsack test case: a label, capacity, items and expected result.
data Case = Case
  { caseName :: String
  , caseCapacity :: Int
  , caseItems :: [(Int, Int)] -- (weight, value)
  , caseExpected :: Int
  }

mkItems :: [(Int, Int)] -> [KnapsackItem]
mkItems = map (\(w, v) -> KnapsackItem {weight = w, value = v})

cases :: [Case]
cases =
  [ Case "empty items" 10 [] 0
  , Case "zero capacity" 0 [(1, 1), (3, 4)] 0
  , Case "single item fits" 5 [(3, 4)] 4
  , Case "single item too heavy" 2 [(3, 4)] 0
  , Case "classic example" 7 [(1, 1), (3, 4), (4, 5), (5, 7)] 9
  , Case "everything fits" 100 [(1, 1), (3, 4), (4, 5), (5, 7)] 17
  , Case "value over weight" 10 [(5, 10), (4, 40), (6, 30), (3, 50)] 90
  , Case "prefer fewer heavy items" 10 [(10, 100), (1, 1), (1, 1)] 100
  , Case "duplicate items" 6 [(2, 3), (2, 3), (2, 3), (2, 3)] 9
  ]

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  results <- forM cases $ \c -> do
    actual <- knapsack (caseCapacity c) (mkItems (caseItems c))
    let ok = actual == caseExpected c
    if ok
      then putStrLn ("PASS knapsack: " ++ caseName c)
      else do
        modifyIORef' failures (+ 1)
        putStrLn $
          "FAIL knapsack: "
            ++ caseName c
            ++ " (expected "
            ++ show (caseExpected c)
            ++ ", got "
            ++ show actual
            ++ ")"
    pure ok
  n <- readIORef failures
  putStrLn $
    "knapsack: "
      ++ show (length (filter id results))
      ++ "/"
      ++ show (length results)
      ++ " passed"
  if n == 0
    then putStrLn "All tests passed."
    else exitFailure
