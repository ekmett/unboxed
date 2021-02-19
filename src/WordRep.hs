{-# Language NoImplicitPrelude #-}
{-# Language RebindableSyntax #-}
{-# Language MagicHash #-}
{-# Language KindSignatures #-}
{-# Language DataKinds #-}
{-# Language TypeSynonymInstances #-}
{-# Language ImportQualifiedPost #-}
{-# Language BangPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module WordRep 
  ( module Unlifted.WordRep
  ) where

import GHC.Prim
import GHC.Integer
import GHC.Types 
import GHC.Enum qualified as G

import Unlifted.WordRep

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
  maxBound = w where !(W# w) = G.maxBound

instance Num Word# where
  (+) = plusWord#
  (-) = minusWord#
  (*) = timesWord#
  negate x = int2Word# (negateInt# (word2Int# x))
  abs x = x 
  signum 0 = 0
  signum _ = 1
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
  minBound = '\0'#
  maxBound = '\x10FFFF'#
