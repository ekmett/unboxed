{-# Language NoImplicitPrelude #-}
{-# Language RebindableSyntax #-}
{-# Language MagicHash #-}
{-# Language KindSignatures #-}
{-# Language DataKinds #-}
{-# Language TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module FloatRep
  ( module Classes.FloatRep
  ) where

import GHC.Prim
import GHC.Integer
import GHC.Types
import Prelude (otherwise)

import Classes.FloatRep

instance Eq Float# where
  x == y = isTrue# (eqFloat# x y)
  x /= y = isTrue# (neFloat# x y)

instance Ord Float# where
  x <= y = isTrue# (leFloat# x y)
  x >= y = isTrue# (geFloat# x y)
  x < y = isTrue# (ltFloat# x y)
  x > y = isTrue# (gtFloat# x y)

instance Num Float# where
  (+) = plusFloat#
  (-) = minusFloat#
  (*) = timesFloat#
  negate = negateFloat#
  abs n
    | n >= 0 = n
    | otherwise = negate n
  signum n
    | n < 0 = negate 1
    | n == 0 = 0
    | otherwise = 1
  fromInteger = floatFromInteger
  {-# INLINE fromInteger #-}
