{-# Language DataKinds #-}
{-# Language KindSignatures #-}
{-# Language MagicHash #-}
{-# Language NoImplicitPrelude #-}
{-# Language RebindableSyntax #-}
{-# Language StandaloneKindSignatures #-}

module Test where

import Data.Kind (Constraint)
import Data.Ratio
import GHC.Integer
import GHC.Prim
import GHC.Types (TYPE, RuntimeRep(..), isTrue#)
import Prelude (otherwise)

type Rep = 'IntRep

infixl 6 +, -
infixl 7 *

type Num :: TYPE Rep -> Constraint
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a

  negate a = 0 - a
  a - b = a + negate b
  {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}

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
