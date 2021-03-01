{-# Language NoImplicitPrelude #-}
{-# Language RebindableSyntax #-}
{-# Language MagicHash #-}
{-# Language KindSignatures #-}
{-# Language DataKinds #-}
{-# Language TypeSynonymInstances #-}
{-# Language ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module W
  ( module Def.W
  , eqWord, neWord, ltWord, leWord, gtWord, geWord
  , eqChar, neChar, ltChar, leChar, gtChar, geChar
  , Word#
  , Char#
  ) where

import Unboxed.Internal.Class
import GHC.Prim
import GHC.Integer
import GHC.Types
import Prelude qualified

import Def.W

eqWord, neWord, ltWord, leWord, gtWord, geWord :: Word# -> Word# -> Bool
eqWord x y = isTrue# (eqWord# x y)
{-# INLINE [1] eqWord #-}
neWord x y = isTrue# (neWord# x y)
{-# INLINE [1] neWord #-}
ltWord x y = isTrue# (ltWord# x y)
{-# INLINE [1] ltWord #-}
gtWord x y = isTrue# (gtWord# x y)
{-# INLINE [1] gtWord #-}
leWord x y = isTrue# (leWord# x y)
{-# INLINE [1] leWord #-}
geWord x y = isTrue# (geWord# x y)
{-# INLINE [1] geWord #-}

instance Eq Word# where
  (==) = eqWord
  (/=) = neWord

instance Ord Word# where
  (<=) = leWord
  (>=) = geWord
  (<) = ltWord
  (>) = gtWord

instance Bounded Word# where
  minBound = case Prelude.minBound of W# i -> i
  maxBound = case Prelude.maxBound of W# i -> i

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

instance Show Word# where
  showsPrec d a = showsPrec d (W# a)
  {-# INLINE showsPrec #-}

eqChar, neChar, ltChar, leChar, gtChar, geChar :: Char# -> Char# -> Bool
eqChar x y = isTrue# (eqChar# x y)
{-# INLINE [1] eqChar #-}
neChar x y = isTrue# (neChar# x y)
{-# INLINE [1] neChar #-}
ltChar x y = isTrue# (ltChar# x y)
{-# INLINE [1] ltChar #-}
gtChar x y = isTrue# (gtChar# x y)
{-# INLINE [1] gtChar #-}
leChar x y = isTrue# (leChar# x y)
{-# INLINE [1] leChar #-}
geChar x y = isTrue# (geChar# x y)
{-# INLINE [1] geChar #-}

instance Eq Char# where
  (==) = eqChar
  (/=) = neChar

instance Ord Char# where
  (<=) = leChar
  (>=) = geChar
  (<) = ltChar
  (>) = gtChar

instance Bounded Char# where
  minBound = '\0'#
  maxBound = '\x10FFFF'#
