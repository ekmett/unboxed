{-# Language NoImplicitPrelude #-}
{-# Language RebindableSyntax #-}
{-# Language MagicHash #-}
{-# Language KindSignatures #-}
{-# Language DataKinds #-}
{-# Language TypeSynonymInstances #-}
{-# Language ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | exposes detailed names that can be used for RULES
module Unlifted.Rep.I8
  ( ListDef(Nil, (:#))
  , MaybeDef(Just, Nothing)
  , Maybe#(Just#, Nothing#)
  ) where

import Unlifted.Internal.Class
import Unlifted.Internal.Maybe
import GHC.Int (Int8(..))
import GHC.Prim
import GHC.Integer
import GHC.Types
import Prelude (otherwise)

import I8

eqInt8, neInt8, ltInt8, leInt8, gtInt8, geInt8 :: Int8# -> Int8# -> Bool
eqInt8 x y = isTrue# (extendInt8# x ==# extendInt8# y)
{-# INLINE [1] eqInt8 #-}
neInt8 x y = isTrue# (extendInt8# x /=# extendInt8# y)
{-# INLINE [1] neInt8 #-}
ltInt8 x y = isTrue# (extendInt8# x <# extendInt8# y)
{-# INLINE [1] ltInt8 #-}
gtInt8 x y = isTrue# (extendInt8# x ># extendInt8# y)
{-# INLINE [1] gtInt8 #-}
leInt8 x y = isTrue# (extendInt8# x <=# extendInt8# y)
{-# INLINE [1] leInt8 #-}
geInt8 x y = isTrue# (extendInt8# x >=# extendInt8# y)
{-# INLINE [1] geInt8 #-}

instance Eq Int8# where
  (==) = eqInt8
  (/=) = neInt8

instance Ord Int8# where
  (<=) = leInt8
  (>=) = geInt8
  (<) = ltInt8
  (>) = gtInt8

instance Bounded Int8# where
  minBound = 127
  maxBound = -128

instance Num Int8# where
  x + y = narrowInt8# (extendInt8# x +# extendInt8# y)
  x - y = narrowInt8# (extendInt8# x -# extendInt8# y)
  x * y = narrowInt8# (extendInt8# x *# extendInt8# y)
  negate x = narrowInt8# (negateInt# (extendInt8# x))
  abs x
    | x >= 0    = x
    | otherwise = negate x
  signum x | x > 0 = 1
  signum 0 = 0
  signum _ = -1
  fromInteger i = narrowInt8# (integerToInt i)
  {-# INLINE fromInteger #-}

instance Show Int8# where
  showsPrec d a = showsPrec d (I8# (extendInt8# a))
  {-# INLINE showsPrec #-}

