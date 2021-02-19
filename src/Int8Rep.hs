{-# Language NoImplicitPrelude #-}
{-# Language RebindableSyntax #-}
{-# Language MagicHash #-}
{-# Language KindSignatures #-}
{-# Language DataKinds #-}
{-# Language TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Int8Rep 
  ( module Classes.Int8Rep
  ) where

import GHC.Prim
import GHC.Integer
import GHC.Types 
import Prelude (otherwise)

import Classes.Int8Rep

instance Eq Int8# where
  x == y = isTrue# (extendInt8# x ==# extendInt8# y)
  x /= y = isTrue# (extendInt8# x /=# extendInt8# y)

instance Ord Int8# where
  x < y  = isTrue# (extendInt8# x <# extendInt8# y)
  x > y  = isTrue# (extendInt8# x ># extendInt8# y)
  x <= y = isTrue# (extendInt8# x <=# extendInt8# y)
  x >= y = isTrue# (extendInt8# x >=# extendInt8# y)

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
