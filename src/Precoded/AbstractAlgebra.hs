module AbstractAlgebra where

import Prelude hiding ((*), (+), (-), (/))
import qualified Prelude as P

newtype Quotient998244353 = Quotient998244353 Int deriving (Eq, Show)

class Field a where
  (+), (*), (-), (/) :: a -> a -> a
  addId, mulId :: a
  addInv, mulInv :: a -> a
  (-) x y = x + addInv y
  (/) x y = x * mulInv y

modP :: Int -> Int
modP x = x `mod` 998244353

powMod :: Int -> Int -> Int -> Int
powMod _ 0 _ = 1
powMod b e m
  | even e = powMod ((b P.* b) `mod` m) (e `div` 2) m
  | otherwise = (b P.* powMod b (e P.- 1) m) `mod` m

instance Field Quotient998244353 where
  (Quotient998244353 a) + (Quotient998244353 b) = Quotient998244353 (modP (a P.+ b))
  (Quotient998244353 a) * (Quotient998244353 b) = Quotient998244353 (modP (a P.* b))
  addId = Quotient998244353 0
  mulId = Quotient998244353 1
  addInv (Quotient998244353 a) = Quotient998244353 (modP (998244353 P.- modP a))
  mulInv (Quotient998244353 a) = Quotient998244353 (powMod (modP a) (998244353 P.- 2) 998244353)
