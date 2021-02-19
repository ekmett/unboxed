{-# Language NoImplicitPrelude #-}
{-# Language RebindableSyntax #-}
{-# Language MagicHash #-}
{-# Language KindSignatures #-}
{-# Language DataKinds #-}
{-# Language TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module WordRep 
  ( module Classes.WordRep
  ) where

import GHC.Prim
import GHC.Wordeger
import GHC.Types 
import Prelude (otherwise)

import Classes.WordRep

instance Eq Word# where
  x == y = isTrue# (x `eqWord#` y)
  x /= y = isTrue# (x `neWord#` y)

instance Ord Word# where
  x <= y = isTrue# (x `leWord#` y)
  x >= y = isTrue# (x `geWord#` y)
  x < y = isTrue# (x `ltWord#` y)
  x > y = isTrue# (x `gtWord#` y)

instance Num Word# where
  (+) = plusWord#
  (-) = minusWord#
  (*) = timesWord#
  negate = negateWord#
  abs n
    | n >= 0 = n
    | otherwise = negate n
  signum n
    | n < 0 = negate 1
    | n == 0 = 0
    | otherwise = 1
  fromInteger = integerToWord
  {-# INLINE fromInteger #-}
