{-# Language NoImplicitPrelude #-}
{-# Language RebindableSyntax #-}
{-# Language MagicHash #-}
{-# Language KindSignatures #-}
{-# Language DataKinds #-}
{-# Language TypeSynonymInstances #-}
{-# Language ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | exposes detailed names that can be used for RULES
module F 
  ( module Def.F
  , eqFloat, neFloat, ltFloat, leFloat, gtFloat, geFloat
  , Float#
  ) where

import GHC.Integer
import GHC.Prim
import GHC.Types
import Prelude (otherwise)
import Unlifted.Internal.Class

import Def.F

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

instance Fractional Float# where
  (/) = divideFloat#
  recip = (/) 1.0#
  fromRational r = case fromRational r of
    F# f -> f

instance Real Float# where
  toRational f = toRational (F# f)
