{-# Language NoImplicitPrelude #-}
{-# Language RebindableSyntax #-}
{-# Language MagicHash #-}
{-# Language KindSignatures #-}
{-# Language DataKinds #-}
{-# Language TypeSynonymInstances #-}
{-# Language BangPatterns #-}
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

instance Bounded Word# where
  minBound = 0
  maxBound = w where !(W# w) = maxBound

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

instance Eq Char# where
  x == y = isTrue# (x `eqChar#` y)
  x /= y = isTrue# (x `neChar#` y)

instance Ord Char# where
  x <= y = isTrue# (x `leChar#` y)
  x >= y = isTrue# (x `geChar#` y)
  x < y = isTrue# (x `ltChar#` y)
  x > y = isTrue# (x `gtChar#` y)

instance Bounded Char# where
  minBound = '\0'
  maxBound = '\x10FFFF'
