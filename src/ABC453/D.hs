module ABC453.D where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Lib (readInts)

type Grid = [[Char]]

newtype Pos = Pos (Int, Int) deriving (Eq, Ord)

newtype BoardSize = BoardSize (Int, Int)

data Dir = U | D | L | R | None deriving (Eq, Ord)

instance Show Dir where
  show U = "U"
  show D = "D"
  show L = "L"
  show R = "R"
  show None = ""

type State = (Pos, Dir)

step :: Pos -> Dir -> Pos
step (Pos (r, c)) U = Pos (r - 1, c)
step (Pos (r, c)) D = Pos (r + 1, c)
step (Pos (r, c)) L = Pos (r, c - 1)
step (Pos (r, c)) R = Pos (r, c + 1)
step pos None = pos

allDirs :: [Dir]
allDirs = [U, D, L, R]

possibleNextStates :: Grid -> (BoardSize) -> State -> [(State, Dir)]
possibleNextStates grid (BoardSize (h, w)) ((Pos (r, c)), prev_dir) =
  let cell = grid !! r !! c
      candidates = case cell of
        'o' -> if prev_dir == None then allDirs else [prev_dir]
        'x' -> filter (/= prev_dir) allDirs
        _ -> allDirs
      valid d =
        let Pos (r', c') = step (Pos (r, c)) d
         in if ( r' >= 0
                   && r' < h
                   && c' >= 0
                   && c' < w
                   && grid !! r' !! c' /= '#'
               )
              then Just (Pos (r', c'))
              else Nothing
   in [((n, d), d) | d <- candidates, Just n <- [valid d]]

data Solution = NoSolution | YesSolution [Dir]

printSolution :: Solution -> IO ()
printSolution NoSolution = putStrLn "No"
printSolution (YesSolution dirs) = do
  putStrLn "Yes"
  putStrLn $ concatMap show dirs

solve :: Grid -> BoardSize -> Solution
solve grid board_size =
  let BoardSize (h, w) = board_size
      findCell ch = head [(r, c) | r <- [0 .. h - 1], c <- [0 .. w - 1], grid !! r !! c == ch]
      start_pos = Pos (findCell 'S')
      goal_pos = Pos (findCell 'G')
      initial_state = (start_pos, None)
      bfs ::
        Seq State ->
        Set State ->
        Map State (State, Dir) ->
        Maybe (Map State (State, Dir))
      bfs Seq.Empty _ _ = Nothing
      bfs (cur :<| queue) seen parent
        | fst cur == goal_pos = Just parent
        | otherwise =
            let next_states = possibleNextStates grid board_size cur
                fresh_states = filter (\(s, _) -> Set.notMember s seen) next_states
                new_seen = foldr (Set.insert . fst) seen fresh_states
                new_parent = foldr (\(s, d) m -> Map.insert s (cur, d) m) parent fresh_states
                new_queue = queue Seq.>< Seq.fromList (map fst fresh_states)
             in bfs new_queue new_seen new_parent
      initial_seen = Set.singleton initial_state
      result = bfs (Seq.singleton initial_state) initial_seen Map.empty
   in case result of
        Nothing -> NoSolution
        Just parent ->
          let recTrace s acc =
                case Map.lookup s parent of
                  Nothing -> acc
                  Just (prev, d) -> recTrace prev (d : acc)
              path = recTrace (goal_pos, None) []
           in YesSolution path

main' :: IO ()
main' = do
  [h, w] <- readInts
  grid <- sequence [getLine | _ <- [1 .. h]]
  printSolution $ solve grid (BoardSize (h, w))
