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
module Unlifted.Rep.Internal.Int where

import Unlifted.Internal.Class
import GHC.Prim
import GHC.Integer
import GHC.Types
import Prelude (otherwise)

import Def.Int ()

eqInt, neInt, ltInt, leInt, gtInt, geInt :: Int# -> Int# -> Bool
eqInt x y = isTrue# (x ==# y)
{-# INLINE [1] eqInt #-}
neInt x y = isTrue# (x /=# y)
{-# INLINE [1] neInt #-}
ltInt x y = isTrue# (x <# y)
{-# INLINE [1] ltInt #-}
gtInt x y = isTrue# (x ># y)
{-# INLINE [1] gtInt #-}
leInt x y = isTrue# (x <=# y)
{-# INLINE [1] leInt #-}
geInt x y = isTrue# (x >=# y)
{-# INLINE [1] geInt #-}

instance Eq Int# where
  (==) = eqInt
  (/=) = neInt

instance Ord Int# where
  (<=) = leInt
  (>=) = geInt
  (<) = ltInt
  (>) = gtInt

{-
instance Bounded Int# where
  minBound = case P.minBound of I# i -> i
  maxBound = case P.maxBound of I# i -> i
-}

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

instance Show Int# where
  showsPrec d a = showsPrec d (I# a)
  {-# INLINE showsPrec #-}
