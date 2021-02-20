{-# Language DataKinds #-}
{-# Language KindSignatures #-}
{-# Language RankNTypes #-}
{-# Language StandaloneKindSignatures #-}
{-# Language MultiParamTypeClasses #-}
{-# Language ImportQualifiedPost #-}
{-# Language MagicHash #-}
{-# Language UnboxedTuples #-}
{-# Language PatternSynonyms #-}
{-# Language NoStarIsType #-}
{-# Language ViewPatterns #-}
{-# Language BangPatterns #-}
{-# Language DefaultSignatures #-}

{-# Language UnboxedSums #-}
{-# Language UnliftedNewtypes #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Types
  ( Liftable(..)
  , pattern Lifted
  , pattern Lowered
  , Lifted#(..)
  , List#(..)
  , IsList(..)
  , Maybe(..)
  , Maybe#(Maybe#, Nothing#, Just#)
  , Num(..)
  , Fractional(..)
  , Floating(..)
  , Real(..)
  , Integral(..)
  , Eq(..)
  , Ord(..)
  , Enum(..)
  , Bounded(..)
  , RealFrac(..)
  , RealFloat(..)
  , Rep
  , ifThenElse
  , Nullary
  ) where

import Data.Kind (Constraint)
import Data.Ratio
import GHC.Base (otherwise)
import GHC.Integer
import GHC.Classes (not)
import GHC.Types (TYPE, Type)
import Prelude qualified as P
import Prelude
  ( Int, Bool(..), Ordering(..)
  , (||), (&&)
  , even, error
  )

import Common
import Rep

--------------------------------------------------------------------------------
-- * Lifted
--------------------------------------------------------------------------------

-- useful for DerivingVia

type Lifted# :: TYPE Rep -> Type
data Lifted# a = Lifted# { lowered# :: a }

instance Eq a => P.Eq (Lifted# a) where
  Lifted# a == Lifted# b = a == b
  Lifted# a /= Lifted# b = a /= b

instance Ord a => P.Ord (Lifted# a) where
  compare (Lifted# a) (Lifted# b) = compare a b

type Liftable :: TYPE Rep -> Constraint
class Liftable a where
  type Lifted :: TYPE Rep -> Type
  type Lifted a = Lifted# a

  lift :: a -> Lifted a 
  default lift :: (Lifted a ~ Lifted# a) => a -> Lifted a
  lift = Lifted#
  
  lower :: Lifted a -> a
  default lower :: (Lifted a ~ Lifted# a) => Lifted a -> a
  lower = lower#

pattern Lifted :: Liftable a => a -> Lifted a
pattern Lifted{lowered} <- (lower -> lowered) where
  Lifted a = lift a
{-# COMPLETE Lifted #-}

pattern Lowered :: Liftable a => Lifted a -> a
pattern Lowered{lifted} <- (lift -> lifted) where
  Lowered a = lower a
{-# COMPLETE Lowered #-}

-- ** Maybe#

newtype Maybe# (a :: TYPE Rep) = Maybe# (# (##) | a #)

pattern Just# :: forall (a :: TYPE Rep). a -> Maybe# a
pattern Just# a = Maybe# (# | a #)

pattern Nothing# :: forall (a :: TYPE Rep). Maybe# a
pattern Nothing# = Maybe# (# (##) | #)

{-# complete Just#, Nothing# #-}

-- ** Maybe

type Maybe :: TYPE Rep -> Type
data Maybe a = Nothing | Just a

instance Eq a => P.Eq (Maybe a) where
  Just a  == Just b  = a == b
  Nothing == Nothing = True
  _       == _       = False

instance Ord a => P.Ord (Maybe a) where
  compare Nothing  Nothing  = EQ
  compare Nothing  Just{}   = LT
  compare Just{}   Nothing  = GT
  compare (Just a) (Just b) = compare a b

-- ** List

type List# :: TYPE Rep -> Type
data List# a = Nil# | a :# List a

infixr 5 :#

instance Eq a => P.Eq (List a) where
  Nil# == Nil# = True
  a :# as == b :# bs = a == b && as P.== bs
  _ == _ = False

  Nil# /= Nil# = False
  a :# as /= b :# bs = a /= b || as P./= bs
  _ /= _ = False

instance Ord a => P.Ord (List a) where
  compare Nil# Nil# = EQ
  compare Nil# (:#){} = LT
  compare (:#){} Nil# = GT
  compare (a :# as) (b :# bs) = compare a b P.<> compare as bs

liftList :: Liftable a => List a -> [Lifted a]
liftList (a :# as) = Lifted a : liftList as
liftList Nil# = []

lowerList :: Liftable a => [Lifted a] -> List a
lowerList (Lifted a : as) = a : lowerList as
lowerList [] = Nil#

--------------------------------------------------------------------------------
-- * RebindableSyntax
--------------------------------------------------------------------------------

-- for rebindable syntax, the "real" ifThenElse works over all kinds
-- but we can't have pretty numbers without this
ifThenElse :: forall (a :: TYPE Rep). Bool -> a -> a -> a
ifThenElse False a _ = a
ifThenElse True _ a = a

-- ** IsList

-- rebindable syntax uses the lifted versions?
type IsList :: Type -> Constraint
class IsList a where
  type Item# :: Type -> TYPE Rep 
  fromList#  :: List (Item# a) -> a
  fromListN# :: Int -> List (Item# a) -> a
  toList#    :: a -> List (Item# a)

  type Item :: Type -> Type
  type Item a = Lifted (Item# a) 
  fromListN :: IsList a => Int -> [Item a] -> a
  fromList  :: IsList a => [Item a] -> a
  toList    :: IsList a => a -> [Item a]

  fromList xs = fromList# (lowerList xs)
  fromList# x = fromList (liftList x)
  fromListN n xs = fromListN# n (lowerList xs)
  fromListN# _ = fromList#
  toList x = liftList (toList# x)
  toList# x = lowerList (toList x)

  {-# MINIMAL (fromList# | fromList), (toList# | toList) -}

--------------------------------------------------------------------------------
-- * Classes
--------------------------------------------------------------------------------

-- ** Eq

infixr 8 **
infixl 7 *, /, `quot`, `rem`, `div`, `mod`
infixl 6 +, -
infix 4 ==, /=, <=, >=, <, >

type Eq :: TYPE Rep -> Constraint
class Eq a where
  (==), (/=) :: a -> a -> Bool
  x == y = not (x /= y)
  x /= y = not (x == y)
  {-# MINIMAL (/=) | (==) #-}

-- ** Ord

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

--------------------------------------------------------------------------------
-- * Bounded
--------------------------------------------------------------------------------

class Nullary
instance Nullary

type Bounded :: TYPE Rep -> Constraint
class Bounded a where
  minBound, maxBound :: Nullary => a
  {-# MINIMAL minBound, maxBound #-}

--------------------------------------------------------------------------------
-- * Num
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- * Fractional
--------------------------------------------------------------------------------

type Fractional :: TYPE Rep -> Constraint
class Num a => Fractional a where
  (/) :: a -> a -> a
  recip :: a -> a
  fromRational :: Rational -> a

  x / y = x * recip y
  recip x = 1 / x
  {-# MINIMAL fromRational, (recip | (/)) #-}

--------------------------------------------------------------------------------
-- * Real
--------------------------------------------------------------------------------

type Real :: TYPE Rep -> Constraint
class (Num a, Ord a) => Real a where
  toRational :: a -> Rational
  {-# MINIMAL toRational #-}

--------------------------------------------------------------------------------
-- * Enum
--------------------------------------------------------------------------------

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

  succ x = toEnum (fromEnum x P.+ One)
  pred x = toEnum (fromEnum x P.- One)
  {-# MINIMAL toEnum, fromEnum #-}

--------------------------------------------------------------------------------
-- * Integral
--------------------------------------------------------------------------------

type Integral :: TYPE Rep -> Constraint
class (Real a, Enum a) => Integral a where
  quot, rem, div, mod :: a -> a -> a
  quotRem, divMod :: a -> a -> (# a, a #)
  toInteger :: a -> Integer
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

  {-# MINIMAL quotRem, toInteger #-}

--------------------------------------------------------------------------------
-- * Floating
--------------------------------------------------------------------------------

-- | Trigonometric and hyperbolic functions and related functions.
type Floating :: TYPE Rep -> Constraint
class Fractional a => Floating a  where
  pi :: Nullary => a
  (**), logBase :: a -> a -> a
  exp, log, sqrt, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh :: a -> a

  {-# INLINE (**) #-}
  {-# INLINE logBase #-}
  {-# INLINE sqrt #-}
  {-# INLINE tan #-}
  {-# INLINE tanh #-}
  x ** y              =  exp (log x * y)
  logBase x y         =  log y / log x
  sqrt x              =  x ** 0.5
  tan  x              =  sin  x / cos  x
  tanh x              =  sinh x / cosh x

  {-# MINIMAL pi, exp, log, sin, cos, asin, acos, atan, sinh, cosh, asinh, acosh, atanh #-}

--------------------------------------------------------------------------------
-- * RealFrac
--------------------------------------------------------------------------------

type RealFrac :: TYPE Rep -> Constraint
class (Real a, Fractional a) => RealFrac a where
  properFraction :: P.Integral b => a -> (# b, a #)
  truncate, round, ceiling, floor :: P.Integral b => a -> b

  truncate x = m where !(# m, _ #) = properFraction x
  {-# INLINE truncate #-}

  round x =
    let !(# n, r #) = properFraction x
        m | r < 0     = n P.- One
          | otherwise = n P.+ One
    in case signum (abs r - 0.5) of
      -1 -> n
      0 | even n -> n
        | otherwise -> m
      1 -> m
      _ -> error "round default defn: Bad value"

  ceiling x
    | r > 0 = n P.+ One
    | otherwise = n
    where !(# n, r #) = properFraction x

  floor x 
    | r < 0 = n P.- One
    | otherwise = n
    where !(# n, r #) = properFraction x

  {-# MINIMAL properFraction #-}

--------------------------------------------------------------------------------
-- * RealFloat
--------------------------------------------------------------------------------

-- | Efficient, machine-independent access to the components of a
-- floating-point number.
type RealFloat :: TYPE Rep -> Constraint
class (RealFrac a, Floating a) => RealFloat a where
  floatRadix :: a -> Integer
  floatDigits, exponent :: a -> Int
  floatRange :: a -> (# Int, Int #)
  decodeFloat :: a -> (# Integer, Int #)
  encodeFloat :: Integer -> Int -> a
  significand :: a -> a
  scaleFloat :: Int -> a -> a
  isNaN, isInfinite, isDenormalized, isNegativeZero, isIEEE :: a -> Bool
  atan2 :: a -> a -> a

  exponent x
    | m P.== Zero = Zero
    | otherwise = n P.+ floatDigits x
    where !(# m, n #) = decodeFloat x

  significand x = encodeFloat m (P.negate (floatDigits x))
    where !(# m, _ #) = decodeFloat x

  scaleFloat Zero x =  x
  scaleFloat k x
    | isFix =  x
    | otherwise =  encodeFloat m (n P.+ clamp b k)
    where
      !(# m, n #) = decodeFloat x
      !(# l, h #) = floatRange x
      d = floatDigits x
      b = h P.- l P.+ Four P.* d
      isFix = x == 0 || isNaN x || isInfinite x

  atan2 y x
    | x > 0            =  atan (y/x)
    | x == 0 && y > 0  =  pi/2
    | x <  0 && y > 0  =  pi + atan (y/x)
    |(x <= 0 && y < 0)            ||
     (x <  0 && isNegativeZero y) ||
     (isNegativeZero x && isNegativeZero y)
                       = -atan2 (-y) x
    | y == 0 && (x < 0 || isNegativeZero x)
                       =  pi    -- must be after the previous test on zero y
    | x==0 && y==0     =  y     -- must be after the other double zero tests
    | otherwise        =  x + y -- x or y is a NaN, return a NaN (via +)

