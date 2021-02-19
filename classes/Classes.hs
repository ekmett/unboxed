{-# Language DataKinds #-}
{-# Language KindSignatures #-}
{-# Language NoImplicitPrelude #-}
{-# Language RankNTypes #-}
{-# Language RebindableSyntax #-}
{-# Language StandaloneKindSignatures #-}

module Classes 
  ( Num(..)
  , Fractional(..)
  , Rep
  , ifThenElse
  ) where

import Data.Kind (Constraint)
import Data.Ratio
import GHC.Integer
import GHC.Types (TYPE, Bool(..))

import Rep

-- for rebindable syntax, the "real" ifThenElse works over all kinds
-- but we can't have pretty numbers without this
ifThenElse :: forall (a :: TYPE Rep). Bool -> a -> a -> a
ifThenElse False a _ = a
ifThenElse True _ a = a

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
