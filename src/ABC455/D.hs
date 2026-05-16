{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ABC455.D where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)

readInts :: IO [Int]
readInts = map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine

readQueries :: (BS.ByteString -> q) -> Maybe Int -> IO [q]
readQueries f (Just n) = map f <$> replicateM n BS.getLine
readQueries f Nothing = map f . BS.lines <$> BS.getContents

solve :: Int -> [(Int, Int)] -> [Int]
solve n ops = elems (runSTUArray (build n ops))

build :: forall s. Int -> [(Int, Int)] -> ST s (STUArray s Int Int)
build n ops = do
  parent <- newArray (1, n) 0 :: ST s (STUArray s Int Int)
  forM_ ops $ \(c, p) -> writeArray parent c p

  root <- newArray (1, n) 0 :: ST s (STUArray s Int Int)

  let resolve :: Int -> ST s Int
      resolve i = do
        r <- readArray root i
        if r /= 0
          then return r
          else do
            par <- readArray parent i
            if par == 0
              then writeArray root i i >> return i
              else do
                r' <- resolve par
                writeArray root i r'
                return r'

  size <- newArray (1, n) 0 :: ST s (STUArray s Int Int)
  forM_ [1 .. n] $ \i -> do
    r <- resolve i
    s <- readArray size r
    writeArray size r (s + 1)
  return size

readOperation :: BS.ByteString -> (Int, Int)
readOperation bs =
  let [c, p] = map (fst . fromJust . BS.readInt) (BS.words bs)
   in (c, p)

printAns :: [Int] -> IO ()
printAns ans = putStrLn $ unwords (map show ans)

main' :: IO ()
main' = do
  [n, q] <- readInts
  operations <- readQueries readOperation (Just q)
  let ans = solve n operations
   in printAns ans
