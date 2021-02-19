{-# Language DataKinds #-}
{-# Language KindSignatures #-}
{-# Language RankNTypes #-}
{-# Language RebindableSyntax #-}
{-# Language StandaloneKindSignatures #-}
{-# Language MultiParamTypeClasses #-}
{-# Language ImportQualifiedPost #-}
{-# Language MagicHash #-}
{-# Language UnboxedTuples #-}
{-# Language PatternSynonyms #-}
{-# Language BangPatterns #-}

{-# Language UnboxedSums #-}
{-# Language UnliftedNewtypes #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Unlifted
  ( Num(..)
  , Fractional(..)
  , Real(..)
  , Integral(..)
  , Eq(..)
  , Ord(..)
  , Enum(..)
  , Bounded(..)
  , Rep
  , ifThenElse
  , Nullary
  , Maybe#(Nothing#, Just#)
  ) where

import Data.Kind (Constraint)
import Data.Ratio
import GHC.Base (otherwise)
import GHC.Integer
import GHC.Classes (not)
import GHC.Types (TYPE, Bool(..), Ordering(..))
import Prelude qualified
import Prelude (Int)

import Common
import Rep

newtype Maybe# (a :: TYPE Rep) = Maybe# (# (##) | a #)

pattern Just# :: forall (a :: TYPE Rep). a -> Maybe# a
pattern Just# a = Maybe# (# | a #)

pattern Nothing# :: forall (a :: TYPE Rep). Maybe# a
pattern Nothing# = Maybe# (# (##) | #)

{-# complete Just#, Nothing# #-}

-- for rebindable syntax, the "real" ifThenElse works over all kinds
-- but we can't have pretty numbers without this
ifThenElse :: forall (a :: TYPE Rep). Bool -> a -> a -> a
ifThenElse False a _ = a
ifThenElse True _ a = a

infixl 6 +, -
infixl 7 *, /, `quot`, `rem`, `div`, `mod`
infix 4 ==, /=, <=, >=, <, >

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

class Nullary
instance Nullary

type Bounded :: TYPE Rep -> Constraint
class Bounded a where
  minBound, maxBound :: Nullary => a
  {-# MINIMAL minBound, maxBound #-}

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

type Real :: TYPE Rep -> Constraint
class (Num a, Ord a) => Real a where
  toRational :: a -> Rational
  {-# MINIMAL toRational #-}

type Enum :: TYPE Rep -> Constraint
class Enum a where
  succ :: a -> a
  pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  -- enumFrom :: a -> [a]
  -- enumFromThen :: a -> a -> [a]
  -- enumFromTo :: a -> a -> [a]
  -- enumFromThenTo :: a -> a -> a -> [a]

  -- enumFrom x             = map toEnum [fromEnum x ..]
  -- enumFromThen x y       = map toEnum [fromEnum x, fromEnum y ..]
  -- enumFromTo x y         = map toEnum [fromEnum x .. fromEnum y]
  -- enumFromThenTo x1 x2 y = map toEnum [fromEnum x1, fromEnum x2 .. fromEnum y]

  succ x = toEnum (fromEnum x Prelude.+ one)
  pred x = toEnum (fromEnum x Prelude.- one)
  {-# MINIMAL toEnum, fromEnum #-}

type Integral :: TYPE Rep -> Constraint
-- Enum a as a superclass
class (Real a, Enum a) => Integral a where
  -- | integer division truncated toward zero
  quot                :: a -> a -> a
  -- | integer remainder, satisfying
  --
  -- > (x `quot` y)*y + (x `rem` y) == x
  rem                 :: a -> a -> a
  -- | integer division truncated toward negative infinity
  div                 :: a -> a -> a
  -- | integer modulus, satisfying
  --
  -- > (x `div` y)*y + (x `mod` y) == x
  mod                 :: a -> a -> a
  -- | simultaneous 'quot' and 'rem'
  quotRem             :: a -> a -> (# a, a #)
  -- | simultaneous 'div' and 'mod'
  divMod              :: a -> a -> (# a, a #)
  -- | conversion to 'Integer'
  toInteger           :: a -> Integer

  {-# INLINE quot #-}
  {-# INLINE rem #-}
  {-# INLINE div #-}
  {-# INLINE mod #-}
  n `quot` d          =  q  where !(# q, _ #) = quotRem n d
  n `rem` d           =  r  where !(# _, r #) = quotRem n d
  n `div` d           =  q  where !(# q, _ #) = divMod n d
  n `mod` d           =  r  where !(# _, r #) = divMod n d

  divMod n d
    | signum r == negate (signum d) = (# q - 1, r + d #)
    | otherwise = qr
    where !qr@(# q, r #) = quotRem n d

