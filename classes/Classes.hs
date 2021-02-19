{-# Language NoImplicitPrelude #-}
{-# Language RebindableSyntax #-}
{-# Language StandaloneKindSignatures #-}
{-# Language DataKinds #-}

module Classes 
  ( Num(..)
  , Fractional(..)
  , Rep
  ) where

import Data.Kind (Constraint)
import Data.Ratio
import GHC.Integer
import GHC.Types (TYPE)

import Rep

infixl 6 +, -
infixl 7 *, /

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

type Fractional :: TYPE Rep -> Constraint
class Num a => Fractional a where
  (/) :: a -> a -> a
  recip :: a -> a
  fromRational :: Rational -> a
  
  x / y = x * recip y
  recip x = 1 / x
  {-# MINIMAL fromRational, (recip | (/)) #-}
