{-# Language CPP #-}
{-# Language NoImplicitPrelude #-}
{-# Language RebindableSyntax #-}
{-# Language MagicHash #-}
{-# Language KindSignatures #-}
{-# Language DataKinds #-}
{-# Language TypeSynonymInstances #-}
{-# Language ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- #include "ieee-flpt.h"
#include "MachDeps.h"

-- | exposes detailed names that can be used for RULES
module Unlifted.Rep.Internal.Float where

import GHC.Integer
import GHC.Prim
import GHC.Types
import Prelude (otherwise)
import Unlifted.Internal.Class

import FloatDef ()

eqFloat_, neFloat_, ltFloat_, leFloat_, gtFloat_, geFloat_ :: Float# -> Float# -> Bool
eqFloat_ x y = isTrue# (eqFloat# x y)
{-# INLINE [1] eqFloat_ #-}
neFloat_ x y = isTrue# (neFloat# x y)
{-# INLINE [1] neFloat_ #-}
ltFloat_ x y = isTrue# (ltFloat# x y)
{-# INLINE [1] ltFloat_ #-}
gtFloat_ x y = isTrue# (gtFloat# x y)
{-# INLINE [1] gtFloat_ #-}
leFloat_ x y = isTrue# (leFloat# x y)
{-# INLINE [1] leFloat_ #-}
geFloat_ x y = isTrue# (geFloat# x y)
{-# INLINE [1] geFloat_ #-}

instance Eq Float# where
  (==) = eqFloat_
  (/=) = neFloat_

instance Ord Float# where
  (<=) = leFloat_
  (>=) = geFloat_
  (<) = ltFloat_
  (>) = gtFloat_

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
