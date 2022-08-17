{-# Language NoImplicitPrelude #-}
{-# Language RebindableSyntax #-}
{-# Language MagicHash #-}
{-# Language KindSignatures #-}
{-# Language DataKinds #-}
{-# Language TypeSynonymInstances #-}
{-# Language ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | exposes detailed names that can be used for RULES
module Unboxed.Rep.Double
  ( module Def.Double
  , eqDouble, neDouble, ltDouble, leDouble, gtDouble, geDouble
  , Double#
  ) where

import Unboxed.Internal.Class
import GHC.Prim
import GHC.Num.Integer
import GHC.Types
import Prelude (otherwise)

import Def.Double

eqDouble, neDouble, ltDouble, leDouble, gtDouble, geDouble :: Double# -> Double# -> Bool
eqDouble x y = isTrue# (x ==## y)
{-# INLINE [1] eqDouble #-}
neDouble x y = isTrue# (x /=## y)
{-# INLINE [1] neDouble #-}
ltDouble x y = isTrue# (x <## y)
{-# INLINE [1] ltDouble #-}
gtDouble x y = isTrue# (x >## y)
{-# INLINE [1] gtDouble #-}
leDouble x y = isTrue# (x <=## y)
{-# INLINE [1] leDouble #-}
geDouble x y = isTrue# (x >=## y)
{-# INLINE [1] geDouble #-}

instance Eq Double# where
  (==) = eqDouble
  (/=) = neDouble

instance Ord Double# where
  (<=) = leDouble
  (>=) = geDouble
  (<) = ltDouble
  (>) = gtDouble

instance Num Double# where
  (+) = (+##)
  (-) = (-##)
  (*) = (*##)
  negate = negateDouble#
  abs n
    | n >= 0 = n
    | otherwise = negate n
  signum n
    | n < 0 = negate 1
    | n == 0 = 0
    | otherwise = 1
  fromInteger n = integerEncodeDouble# n 0#
  {-# INLINE fromInteger #-}

instance Fractional Double# where
  (/) = (/##)
  recip = (/) 1.0##
  fromRational r = case fromRational r of
    D# f -> f

instance Real Double# where
  toRational f = toRational (D# f)
