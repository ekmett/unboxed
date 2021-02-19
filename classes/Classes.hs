{-# Language DataKinds #-}
{-# Language KindSignatures #-}
{-# Language NoImplicitPrelude #-}
{-# Language RankNTypes #-}
{-# Language RebindableSyntax #-}
{-# Language StandaloneKindSignatures #-}

module Classes
  ( Num(..)
  , Fractional(..)
  , Eq(..)
  , Ord(..)
  , Rep
  , ifThenElse
  ) where

import Data.Kind (Constraint)
import Data.Ratio
import GHC.Base (otherwise)
import GHC.Integer
import GHC.Classes (not)
import GHC.Types (TYPE, Bool(..), Ordering(..))

import Rep

-- for rebindable syntax, the "real" ifThenElse works over all kinds
-- but we can't have pretty numbers without this
ifThenElse :: forall (a :: TYPE Rep). Bool -> a -> a -> a
ifThenElse False a _ = a
ifThenElse True _ a = a

infixl 6 +, -
infixl 7 *, /
infix 4 ==, /=

type Eq :: TYPE Rep -> Constraint
class Eq a where
  (==), (/=) :: a -> a -> Bool
  x == y = not (x /= y)
  x /= y = not (x == y)
  {-# MINIMAL (/=) | (==) #-}

type Ord :: TYPE Rep -> Constraint
class Eq a => Ord a where
  (<), (>), (<=), (>=) :: a -> a -> Bool
  compare :: a -> a -> Ordering
  max, min :: a -> a -> a

  compare x y
    | x == y = EQ
    | x <= y = LT
    | otherwise = GT

  x <  y = case compare x y of { LT -> True;  _ -> False }
  x <= y = case compare x y of { GT -> False; _ -> True }
  x >  y = case compare x y of { GT -> True;  _ -> False }
  x >= y = case compare x y of { LT -> False; _ -> True }

  max x y
    | x <= y = y
    | otherwise = x

  min x y
    | x <= y = x
    | otherwise = y
  {-# MINIMAL compare | (<=) #-}

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
