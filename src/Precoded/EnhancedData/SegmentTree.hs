{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Precoded.EnhancedData.SegmentTree where

import Data.FingerTree (FingerTree, Measured (..))
import qualified Data.FingerTree as FT

newtype SegElem a = SegElem {getElem :: a} deriving (Show)

instance (Measured v a) => Measured v (SegElem a) where
  measure (SegElem x) = measure x

type SegTree v a = FingerTree v (SegElem a)

fromList :: (Measured v a) => [a] -> SegTree v a
fromList = FT.fromList . map SegElem
