module ZZZ.Compress0413 (main) where

import Data.Complex (Complex (..), magnitude, realPart)
import Data.List (sortBy)
import Data.Ord (comparing, Down (..))

-- | Cooley-Tukey FFT (radix-2, requires length = power of 2)
fft :: [Complex Double] -> [Complex Double]
fft [] = []
fft [x] = [x]
fft xs
  | even n    = combine (fft evens) (fft odds)
  | otherwise = error "fft: length must be a power of 2"
  where
    n      = length xs
    evens  = [xs !! i | i <- [0, 2 .. n - 1]]
    odds   = [xs !! i | i <- [1, 3 .. n - 1]]
    -- twiddle factor: e^{-2*pi*i*k/n}
    tw k   = let angle = -2 * pi * fromIntegral k / fromIntegral n
             in  cos angle :+ sin angle
    combine es os =
      let half = n `div` 2
          left  = [es !! k + tw k * os !! k | k <- [0 .. half - 1]]
          right = [es !! k - tw k * os !! k | k <- [0 .. half - 1]]
      in  left ++ right

-- | Inverse FFT via conjugate trick: ifft x = conj(fft(conj(x))) / n
ifft :: [Complex Double] -> [Complex Double]
ifft xs =
  let n    = length xs
      conj (r :+ i) = r :+ (-i)
      scaled = map (\x -> x / fromIntegral n) (fft (map conj xs))
  in  map conj scaled

-- | Zero-pad to next power of two
padToPow2 :: [Complex Double] -> [Complex Double]
padToPow2 xs =
  let n    = length xs
      n'   = nextPow2 n
  in  xs ++ replicate (n' - n) 0

nextPow2 :: Int -> Int
nextPow2 n = head [p | p <- iterate (* 2) 1, p >= n]

-- | Full pipeline: parse, compress, report
run :: String -> Int -> IO ()
run input keepN = do
  let vals = parseFloats input
      n    = length vals
      padded  = padToPow2 (map (:+ 0) vals)
      freqs   = fft padded
      indexed = zip [0 ..] freqs
      sorted  = sortBy (comparing (Down . magnitude . snd)) indexed
      kept    = take keepN sorted
      keptSet = map fst kept
      zeroed  = [ if i `elem` keptSet then freqs !! i else 0
                | i <- [0 .. length freqs - 1] ]
      recon        = map realPart (ifft zeroed)
      reconTrimmed = take n recon
      loss = sqrt $ (1 / fromIntegral n)
               * sum [(vals !! i - reconTrimmed !! i) ^ (2 :: Int) | i <- [0 .. n - 1]]
  putStrLn $ "Original length : " ++ show n
  putStrLn $ "Compressed length: " ++ show keepN
                ++ "  (complex coefficients stored)"
  putStrLn $ "Compression ratio: "
                ++ show (fromIntegral keepN / fromIntegral n :: Double)
  putStrLn $ "RMSE loss        : " ++ show loss

-- | Parse comma-separated floats
parseFloats :: String -> [Double]
parseFloats s = map read (splitOn ',' (filter (/= '\n') s))

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn c str =
  let (tok, rest) = break (== c) str
  in  tok : case rest of
              []     -> []
              (_:rs) -> splitOn c rs

main :: IO ()
main = do
  input <- readFile "inputs/ZZZ/Compress0413/random_string"
  let vals = parseFloats input
      n    = length vals
  putStrLn $ "=== FFT Compression Demo ==="
  putStrLn $ "Signal length: " ++ show n
  putStrLn ""
  -- Try several compression ratios
  mapM_ (\r -> do
    let keepN = max 1 (round (fromIntegral n * r :: Double))
    putStrLn $ "--- Keep ratio " ++ show r ++ " ---"
    run input keepN
    putStrLn ""
    ) [0.5, 0.25, 0.1, 0.05 :: Double]
