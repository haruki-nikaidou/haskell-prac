module Main (main) where

import Control.Monad (forM)
import Data.IORef
import qualified Data.Map.Strict as Map
import Precoded.Algorithm.DP (KnapsackItem (..), knapsack)
import Precoded.EnhancedData.ChthollyTree (ChthollyTree, RangeNode (RangeNode), getTree)
import qualified Precoded.EnhancedData.ChthollyTree as CT
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

-- | Structural invariant of a well-formed Chtholly tree:
--   * every node is keyed by its own left endpoint,
--   * every node covers a non-empty half-open interval (l < r),
--   * adjacent nodes never overlap (prev.right <= next.left).
isValid :: ChthollyTree a -> Bool
isValid t =
  keysOk && nonEmpty && noOverlap
  where
    assocs = Map.toList (getTree t)
    nodes = map snd assocs
    keysOk = all (\(k, RangeNode l _ _) -> k == l) assocs
    nonEmpty = all (\(RangeNode l r _) -> l < r) nodes
    noOverlap =
      and (zipWith (\(RangeNode _ r _) (RangeNode l' _ _) -> r <= l') nodes (drop 1 nodes))

-- | Pure ChthollyTree assertions: (label, did it pass?).
ctCases :: [(String, Bool)]
ctCases =
  [ -- toList enumerates each covered index in order.
    ( "toList of contiguous ranges"
    , CT.toList contiguous
        == [(0, 'a'), (1, 'a'), (2, 'a'), (3, 'b'), (4, 'b'), (5, 'c'), (6, 'c'), (7, 'c')]
    )
  , ("contiguous tree is valid", isValid contiguous)
  , ("lookup inside a range", CT.lookup 4 contiguous == Just 'b')
  , ("lookup at a boundary", CT.lookup 5 contiguous == Just 'c')
  , ("lookup outside any range", CT.lookup 8 contiguous == Nothing)
  , -- assign overwrites a sub-range and keeps the tree well-formed.
    ( "assign overwrites a sub-range"
    , CT.toList (CT.assign 2 5 'z' contiguous)
        == [(0, 'a'), (1, 'a'), (2, 'z'), (3, 'z'), (4, 'z'), (5, 'c'), (6, 'c'), (7, 'c')]
    )
  , ("assigned tree is valid", isValid (CT.assign 2 5 'z' contiguous))
  , -- The following exercise fromRangeList with OVERLAPPING inputs.
    -- These currently fail: fromRangeList only does `Map.insert left node`,
    -- so it neither splits nor overwrites overlapping nodes.
    ("overlapping fromRangeList is valid", isValid overlapping)
  , ( "overlapping fromRangeList splits/overwrites (first range wins)"
    , CT.toList overlapping
        == [(0, 'a'), (1, 'a'), (2, 'a'), (3, 'a'), (4, 'a'), (5, 'b'), (6, 'b')]
    )
  , ("nested fromRangeList is valid", isValid nested)
  , ( "nested fromRangeList carves the inner range out"
    , CT.toList nested
        == [(0, 'a'), (1, 'a'), (2, 'a'), (3, 'b'), (4, 'b'), (5, 'b'), (6, 'a'), (7, 'a'), (8, 'a'), (9, 'a')]
    )
  ]
  where
    contiguous =
      CT.fromRangeList [((0, 3), 'a'), ((3, 5), 'b'), ((5, 8), 'c')]
    -- [0,5) painted 'a', then [2,7) painted 'b'; with foldr the head wins,
    -- so the desired result keeps 'a' on [0,5) and 'b' only on [5,7).
    overlapping =
      CT.fromRangeList [((0, 5), 'a'), ((2, 7), 'b')]
    -- A wide 'a' range with a narrow 'b' range carved out of its middle.
    nested =
      CT.fromRangeList [((3, 6), 'b'), ((0, 10), 'a')]

runCt :: IORef Int -> IO [Bool]
runCt failures =
  forM ctCases $ \(name, ok) -> do
    if ok
      then putStrLn ("PASS chtholly: " ++ name)
      else do
        modifyIORef' failures (+ 1)
        putStrLn ("FAIL chtholly: " ++ name)
    pure ok

runKnapsack :: IORef Int -> IO [Bool]
runKnapsack failures =
  forM cases $ \c -> do
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

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)

  knapsackResults <- runKnapsack failures
  putStrLn $
    "knapsack: "
      ++ show (length (filter id knapsackResults))
      ++ "/"
      ++ show (length knapsackResults)
      ++ " passed"

  ctResults <- runCt failures
  putStrLn $
    "chtholly: "
      ++ show (length (filter id ctResults))
      ++ "/"
      ++ show (length ctResults)
      ++ " passed"

  n <- readIORef failures
  if n == 0
    then putStrLn "All tests passed."
    else exitFailure
