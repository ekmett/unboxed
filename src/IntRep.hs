{-# Language NoImplicitPrelude #-}
{-# Language RebindableSyntax #-}
{-# Language MagicHash #-}
{-# Language KindSignatures #-}
{-# Language DataKinds #-}
{-# Language TypeSynonymInstances #-}
{-# Language ImportQualifiedPost #-}
{-# Language BangPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IntRep
  ( module Classes.IntRep
  ) where

import GHC.Prim
import GHC.Integer
import GHC.Types
import GHC.Enum qualified as G
import Prelude (otherwise)

import Classes.IntRep

instance Eq Int# where
  x == y = isTrue# (x ==# y)
  x /= y = isTrue# (x /=# y)

instance Ord Int# where
  x <= y = isTrue# (x <=# y)
  x >= y = isTrue# (x >=# y)
  x < y = isTrue# (x <# y)
  x > y = isTrue# (x ># y)

instance Bounded Int# where
  minBound = i where !(I# i) = G.minBound
  maxBound = i where !(I# i) = G.maxBound

instance Num Int# where
  (+) = (+#)
  (-) = (-#)
  (*) = (*#)
  negate = negateInt#
  abs n
    | n >= 0 = n
    | otherwise = negate n
  signum n
    | n < 0 = negate 1
    | n == 0 = 0
    | otherwise = 1
  fromInteger = integerToInt
  {-# INLINE fromInteger #-}
