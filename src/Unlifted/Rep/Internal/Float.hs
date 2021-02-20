{-# Language NoImplicitPrelude #-}
{-# Language RebindableSyntax #-}
{-# Language MagicHash #-}
{-# Language KindSignatures #-}
{-# Language DataKinds #-}
{-# Language TypeSynonymInstances #-}
{-# Language ImportQualifiedPost #-}
{-# Language BangPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | exposes detailed names that can be used for RULES
module Unlifted.Rep.Internal.Float where

import Unlifted.Internal.Class
import GHC.Prim
import GHC.Integer
import GHC.Types
import Prelude (otherwise)

import FloatDef ()

eqFloat, neFloat, ltFloat, leFloat, gtFloat, geFloat :: Float# -> Float# -> Bool
eqFloat x y = isTrue# (eqFloat# x y)
{-# INLINE [1] eqFloat #-}
neFloat x y = isTrue# (neFloat# x y)
{-# INLINE [1] neFloat #-}
ltFloat x y = isTrue# (ltFloat# x y)
{-# INLINE [1] ltFloat #-}
gtFloat x y = isTrue# (gtFloat# x y)
{-# INLINE [1] gtFloat #-}
leFloat x y = isTrue# (leFloat# x y)
{-# INLINE [1] leFloat #-}
geFloat x y = isTrue# (geFloat# x y)
{-# INLINE [1] geFloat #-}

instance Eq Float# where
  (==) = eqFloat
  (/=) = neFloat

instance Ord Float# where
  (<=) = leFloat
  (>=) = geFloat
  (<) = ltFloat
  (>) = gtFloat

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
