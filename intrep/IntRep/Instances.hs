{-# Language NoImplicitPrelude #-}
{-# Language RebindableSyntax #-}
{-# Language MagicHash #-}
{-# Language KindSignatures #-}
{-# Language DataKinds #-}

module IntRep.Instances where

import GHC.Prim
import GHC.Integer
import IntRep.Rep
import IntRep.Classes
import IntRep.Syntax

instance Num Int# where
  (+) = (+#)
  (-) = (-#)
  (*) = (*#)
  negate = negateInt#
  abs n 
    | n >=# 0 = n 
    | otherwise = negate n
  signum n
    | n <# 0 = negate 1
    | n ==# 0 = 0
    | otherwise = 1
  fromInteger = integerToInt
  {-# INLINE fromInteger #-}
