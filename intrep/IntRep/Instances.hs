{-# Language NoImplicitPrelude #-}
{-# Language RebindableSyntax #-}
{-# Language MagicHash #-}
{-# Language KindSignatures #-}
{-# Language DataKinds #-}
{-# Language TypeSynonymInstances #-}

module IntRep.Instances where

import GHC.Prim
import GHC.Integer
import GHC.Types 
import IntRep.Classes
import Prelude (otherwise)

instance Num Int# where
  (+) = (+#)
  (-) = (-#)
  (*) = (*#)
  negate = negateInt#
  abs n
    | isTrue# (n >=# 0) = n
    | otherwise = negate n
  signum n
    | isTrue# (n <# 0) = negate 1
    | isTrue# (n ==# 0) = 0
    | otherwise = 1
  fromInteger = integerToInt
  {-# INLINE fromInteger #-}
