{-# Language ConstrainedClassMethods #-}
{-# Language DataKinds #-}
{-# Language DefaultSignatures #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language ImportQualifiedPost #-}
{-# Language NoImplicitPrelude #-}
{-# Language PolyKinds #-}
{-# Language RankNTypes #-}
{-# Language RebindableSyntax #-}
{-# Language ScopedTypeVariables #-}
{-# Language InstanceSigs #-}
{-# Language StandaloneKindSignatures #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}
{-# Language UnboxedSums #-}
{-# Language UnboxedTuples #-}
{-# Language UndecidableInstances #-}
{-# Language UndecidableSuperClasses #-}
{-# OPTIONS_HADDOCK not-home #-}

module Unboxed.Internal.Class
  ( Eq(..), EqRep(..)
  , Ord(..), OrdRep(..)
  , Num(..), NumRep(..)
  , Fractional(..), FractionalRep(..)
  , Real(..), RealRep(..)
  , Enum(..), EnumRep(..)
  , Integral(..), IntegralRep(..)
  , Floating(..), FloatingRep(..)
  , RealFrac(..), RealFracRep(..)
  , RealFloat(..), RealFloatRep(..)
  , Bounded(..)
  -- * Show
  , Show(..), ShowList(..), ShowRep(..), ShowListRep(..), shows
  -- * Semigroup
  , Semigroup(..)
  -- * Monoid
  , Monoid(..)
  -- * Functor
  , Functor(..)
  -- * polykinded @hPrint@, @print@
  , PrintRep(hPrint), print
  ) where

import Data.Proxy
import Data.Kind (Constraint)
import Data.Ratio (Rational)
import GHC.Integer
import GHC.Prim
import GHC.Types (Type, RuntimeRep(..), LiftedRep)
import Numeric qualified
import Prelude (Ordering(..), Bool(..), Int, ShowS, String, IO)
import Prelude qualified
import Unboxed.Internal.Levitation
import Unboxed.Internal.List
import Unboxed.Internal.Maybe
import Unboxed.Internal.Rebind
import System.IO qualified as IO

-- * Standard Classes

-- ** Eq


class Eq (a :: TYPE r) where
  (==), (/=) :: a -> a -> Bool

  default (==) :: EqRep r => a -> a -> Bool
  (==) = eqDef

  default (/=) :: EqRep r => a -> a -> Bool
  (/=) = neDef
  {-# MINIMAL (/=) | (==) #-}

infix 4 ==, /=

instance Prelude.Eq a => Eq (a :: Type) where
  (==) = (Prelude.==)
  (/=) = (Prelude./=)

class EqRep (r :: RuntimeRep) where
  eqDef, neDef :: forall (a :: TYPE r). Eq a => a -> a -> Bool

-- ** Ord

class Eq a => Ord (a :: TYPE r) where
  (<), (>), (<=), (>=) :: a -> a -> Bool
  compare :: a -> a -> Ordering
  max, min :: a -> a -> a

  default (<) :: OrdRep r => a -> a -> Bool
  (<) = ltDef

  default (>) :: OrdRep r => a -> a -> Bool
  (>) = gtDef

  default (<=) :: OrdRep r => a -> a -> Bool
  (<=) = leDef

  default (>=) :: OrdRep r => a -> a -> Bool
  (>=) = geDef

  default compare :: OrdRep r => a -> a -> Ordering
  compare = compareDef

  default max :: OrdRep r => a -> a -> a
  max = maxDef

  default min :: OrdRep r => a -> a -> a
  min = minDef

  {-# MINIMAL compare | (<=) #-}

instance Prelude.Ord a => Ord (a :: Type) where
  (<) = (Prelude.<)
  (>) = (Prelude.>)
  (<=) = (Prelude.<=)
  (>=) = (Prelude.>=)
  compare = Prelude.compare
  min = Prelude.min
  max = Prelude.min

infix 4 <=, >=, <, >

class OrdRep (r :: RuntimeRep) where
  compareDef :: forall (a :: TYPE r). Ord a => a -> a -> Ordering
  ltDef, leDef, geDef, gtDef :: forall (a :: TYPE r). Ord a => a -> a -> Bool
  maxDef, minDef :: forall (a :: TYPE r). Ord a => a -> a -> a

-- ** Bounded

class Bounded (a :: TYPE r) where
  minBound, maxBound :: Lev a

instance Prelude.Bounded a => Bounded (a :: Type) where
  minBound = Prelude.minBound
  maxBound = Prelude.maxBound

-- ** Num

infixl 6 +, -
infixl 7 *

class Num (a :: TYPE r) where
  (+),(-),(*) :: a -> a -> a
  negate, abs, signum :: a -> a
  fromInteger :: Integer -> a

  default negate :: NumRep r => a -> a
  negate = negateDef

  default (-) :: NumRep r => a -> a -> a
  (-) = minusDef
  {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}

-- compatible with default definitions
instance Prelude.Num a => Num (a :: Type) where
  (+) = (Prelude.+)
  (-) = (Prelude.-)
  (*) = (Prelude.*)
  negate = Prelude.negate
  abs = Prelude.abs
  signum = Prelude.signum
  fromInteger = Prelude.fromInteger

class NumRep (r :: RuntimeRep) where
  negateDef :: forall (a :: TYPE r). Num a => a -> a
  minusDef :: forall (a :: TYPE r). Num a => a -> a -> a

-- ** Fractional

infixl 7 /

class Num a => Fractional (a :: TYPE r) where
  (/) :: a -> a -> a
  recip :: a -> a
  fromRational :: Rational -> a

  default (/) :: FractionalRep r => a -> a -> a
  (/) = fractionalDef

  default recip :: FractionalRep r => a -> a
  recip = recipDef

  {-# MINIMAL fromRational, (recip | (/)) #-}

class FractionalRep (r :: RuntimeRep) where
  fractionalDef :: forall (a :: TYPE r). Fractional a => a -> a -> a
  recipDef :: forall (a :: TYPE r). Fractional a => a -> a

instance Prelude.Fractional a => Fractional (a :: Type) where
  (/) = (Prelude./)
  recip = Prelude.recip
  fromRational = Prelude.fromRational

-- ** Real

class (Num a, Ord a) => Real (a :: TYPE r) where
  toRational :: a -> Rational

  -- bolted on to class to allow both Real and Frac to be polymorphic in rep
  realToFrac :: Fractional b => a -> b
  default realToFrac :: (RealRep r, Fractional b) => a -> b
  realToFrac = realToFracDef

  {-# MINIMAL toRational #-}

instance Prelude.Real a => Real (a :: Type) where
  toRational = Prelude.toRational
  realToFrac x = fromRational (toRational x)

class RealRep (r :: RuntimeRep) where
  realToFracDef :: forall (a :: TYPE r) s (b :: TYPE s). (Real a, Fractional b) => a -> b

-- ** Enum

class Enum (a :: TYPE r) where
  succ :: a -> a
  default succ :: EnumRep r => a -> a
  succ = succDef

  pred :: a -> a
  default pred :: EnumRep r => a -> a
  pred = succDef

  toEnum :: Int -> a

  fromEnum :: a -> Int

  {-# MINIMAL toEnum, fromEnum #-}

{-
  enumFrom :: a -> List a
  default enumFrom :: EnumRep r => a -> List a
  enumFrom = enumFromDef

  enumFromThen :: a -> a -> List a
  default enumFromThen :: EnumRep r => a -> a -> List a
  enumFromThen = enumFromThenDef

  enumFromTo :: a -> a -> List a
  default enumFromTo :: EnumRep r => a -> a -> List a
  enumFromTo = enumFromToDef

  enumFromThenTo :: a -> a -> a -> List a
  default enumFromThenTo :: EnumRep r => a -> a -> a -> List a
  enumFromThenTo = enumFromThenToDef
-}

class EnumRep (r :: RuntimeRep) where
  succDef, predDef :: forall (a :: TYPE r). Enum a => a -> a
{-
  enumFromDef :: forall (a :: TYPE r). Enum a => a -> List a
  enumFromThenDef, enumFromToDef :: forall (a :: TYPE r). Enum a => a -> a -> List a
  enumFromThenToDef :: forall (a :: TYPE r). Enum a => a -> a -> a -> List a
-}

instance Prelude.Enum a => Enum (a :: Type) where
  succ = Prelude.succ
  pred = Prelude.pred
  toEnum = Prelude.toEnum
  fromEnum = Prelude.fromEnum
{-
  enumFrom = Prelude.enumFrom
  enumFromThen = Prelude.enumFromThen
  enumFromTo = Prelude.enumFromTo
  enumFromThenTo = Prelude.enumFromThenTo
-}

-- ** Integral

class (Real a, Enum a) => Integral (a :: TYPE r) where
  quot, rem, div, mod :: a -> a -> a
  {-# INLINE quot #-}
  {-# INLINE rem #-}
  {-# INLINE div #-}
  {-# INLINE mod #-}

  quotRem, divMod :: a -> a -> (# a, a #)
  toInteger :: a -> Integer

  default quot :: IntegralRep r => a -> a -> a
  quot = quotDef

  default rem :: IntegralRep r => a -> a -> a
  rem = remDef

  default div :: IntegralRep r => a -> a -> a
  div = divDef

  default mod :: IntegralRep r => a -> a -> a
  mod = modDef

  default divMod:: IntegralRep r => a -> a -> (# a, a #)
  divMod = divModDef

  {-# MINIMAL quotRem, toInteger #-}

instance Prelude.Integral a => Integral (a :: Type) where
  quot = Prelude.quot
  rem = Prelude.rem
  div = Prelude.div
  mod = Prelude.mod
  quotRem x y = case Prelude.quotRem x y of
    (q,r) -> (# q, r #)
  {-# INLINE quotRem #-}
  divMod x y = case Prelude.divMod x y of
    (d,m) -> (# d, m #)
  {-# INLINE divMod #-}
  toInteger = Prelude.toInteger

class IntegralRep (r :: RuntimeRep) where
  quotDef, remDef, divDef, modDef :: forall (a :: TYPE r). Integral a => a -> a -> a
  divModDef :: forall (a :: TYPE r). Integral a => a -> a -> (# a, a #)


class (Real a, Fractional a) => RealFrac (a :: TYPE r) where
  -- | As a technical wibble you'll have to convert from the
  -- Prelude fractional type yourself, to avoid n^2 work
  -- making these instances.
  properFraction :: Integral b => a -> (# b, a #)

  truncate, round, ceiling, floor :: Integral b => a -> b
  {-# MINIMAL properFraction #-}

  default truncate :: (RealFracRep r, Integral b) => a -> b
  truncate = truncateDef

  default round :: (RealFracRep r, Integral b) => a -> b
  round = roundDef

  default ceiling :: (RealFracRep r, Integral b) => a -> b
  ceiling = ceilingDef

  default floor :: (RealFracRep r, Integral b) => a -> b
  floor = floorDef

instance Prelude.RealFrac a => RealFrac (a :: Type) where
  properFraction a = case Prelude.properFraction a of
    (b, c) -> (# fromInteger b, c #)

  truncate x = fromInteger (Prelude.truncate x)
  round x = fromInteger (Prelude.round x)
  ceiling x = fromInteger (Prelude.ceiling x)
  floor x = fromInteger (Prelude.floor x)


class RealFracRep (r :: RuntimeRep) where
  truncateDef, roundDef, ceilingDef, floorDef
    :: forall (a :: TYPE r) r' (b :: TYPE r'). (RealFrac a, Integral b)
    => a -> b
--------------------------------------------------------------------------------
-- * Floating
--------------------------------------------------------------------------------

class Fractional a => Floating (a :: TYPE r) where
  pi                  :: Lev a
  exp, log, sqrt      :: a -> a
  (**), logBase       :: a -> a -> a
  sin, cos, tan       :: a -> a
  asin, acos, atan    :: a -> a
  sinh, cosh, tanh    :: a -> a
  asinh, acosh, atanh :: a -> a
  log1p               :: a -> a
  expm1               :: a -> a
  log1pexp            :: a -> a
  log1mexp            :: a -> a

  default (**) :: FloatingRep r => a -> a -> a
  (**) = powDef
  {-# INLINE (**) #-}

  default logBase :: FloatingRep r => a -> a -> a
  logBase = logBaseDef
  {-# INLINE logBase #-}

  default sqrt :: FloatingRep r => a -> a
  sqrt = sqrtDef
  {-# INLINE sqrt #-}

  default tan :: FloatingRep r => a -> a
  tan = tanDef
  {-# INLINE tan #-}

  default tanh :: FloatingRep r => a -> a
  tanh = tanhDef
  {-# INLINE tanh #-}

  default log1p :: FloatingRep r => a -> a
  log1p = log1pDef
  {-# INLINE log1p #-}

  default expm1 :: FloatingRep r => a -> a
  expm1 = expm1Def
  {-# INLINE expm1 #-}

  default log1pexp :: FloatingRep r => a -> a
  log1pexp = log1pexpDef
  {-# INLINE log1pexp #-}

  default log1mexp :: FloatingRep r => a -> a
  log1mexp = log1mexpDef
  {-# INLINE log1mexp #-}

instance Prelude.Floating a => Floating a where
  pi = Prelude.pi
  exp = Prelude.exp
  log = Prelude.log
  sqrt = Prelude.sqrt
  (**) = (Prelude.**)
  logBase = Prelude.logBase
  sin = Prelude.sin
  cos = Prelude.cos
  tan = Prelude.tan
  asin = Prelude.asin
  acos = Prelude.acos
  atan = Prelude.atan
  sinh = Prelude.sinh
  cosh = Prelude.cosh
  tanh = Prelude.tanh
  asinh = Prelude.asinh
  acosh = Prelude.acosh
  atanh = Prelude.atanh
  log1p = Numeric.log1p
  expm1 = Numeric.expm1
  log1pexp = Numeric.log1pexp
  log1mexp = Numeric.log1mexp

class FloatingRep (r :: RuntimeRep) where
  powDef, logBaseDef
    :: forall (a :: TYPE r). Floating a => a -> a -> a

  sqrtDef, tanDef, tanhDef, log1pDef, expm1Def, log1pexpDef, log1mexpDef
    :: forall (a :: TYPE r). Floating a => a -> a

--------------------------------------------------------------------------------
-- * RealFloat
--------------------------------------------------------------------------------

-- | Efficient, machine-independent access to the components of a
-- floating-point number.
class (RealFrac a, Floating a) => RealFloat (a :: TYPE r) where
  floatRadix :: a -> Integer
  floatDigits, exponent :: a -> Int
  floatRange :: a -> (# Int, Int #)
  decodeFloat :: a -> (# Integer, Int #)
  encodeFloat :: Integer -> Int -> a
  significand :: a -> a
  scaleFloat :: Int -> a -> a
  isNaN, isInfinite, isDenormalized, isNegativeZero, isIEEE :: a -> Bool
  atan2 :: a -> a -> a

  default exponent :: RealFloatRep r => a -> Int
  exponent = exponentDef

  default significand :: RealFloatRep r => a -> a
  significand = significandDef

  default scaleFloat :: RealFloatRep r => Int -> a -> a
  scaleFloat = scaleFloatDef

  default atan2 :: RealFloatRep r => a -> a -> a
  atan2 = atan2Def

  {-# MINIMAL
     floatRadix, floatDigits, floatRange, decodeFloat,
     encodeFloat, isNaN, isInfinite, isDenormalized,
     isNegativeZero, isIEEE #-}

instance Prelude.RealFloat a => RealFloat (a :: Type) where
  floatRadix = Prelude.floatRadix
  floatDigits = Prelude.floatDigits
  exponent = Prelude.exponent
  floatRange a = case Prelude.floatRange a of (b, c) -> (# b, c #)
  decodeFloat a = case Prelude.decodeFloat a of (m, e) ->  (# m, e #)
  encodeFloat = Prelude.encodeFloat
  significand = Prelude.significand
  scaleFloat = Prelude.scaleFloat
  isNaN = Prelude.isNaN
  isInfinite = Prelude.isInfinite
  isDenormalized = Prelude.isDenormalized
  isNegativeZero = Prelude.isNegativeZero
  isIEEE = Prelude.isIEEE
  atan2 = Prelude.atan2

class RealFloatRep r where
  exponentDef :: forall (a :: TYPE r). RealFloat a => a -> Int
  significandDef :: forall (a :: TYPE r). RealFloat a => a -> a
  scaleFloatDef :: forall (a :: TYPE r). RealFloat a => Int -> a -> a
  atan2Def :: forall (a :: TYPE r). RealFloat a => a -> a -> a

-- ** Semigroup

class Semigroup (a :: TYPE r) where
  (<>) :: a -> a -> a
  -- sconcat :: NonEmpty a -> a
  -- stimes :: Integral b => b -> a -> a -- probably needs prelude Integral

instance Prelude.Semigroup a => Semigroup a where
  (<>) = (Prelude.<>)

-- ** Monoid

type Monoid :: TYPE r -> Constraint
class Semigroup a => Monoid (a :: TYPE r) where
  mempty :: Lev a

instance Prelude.Monoid a => Monoid a where
  mempty = Prelude.mempty

class Show (a :: TYPE r) where
  showsPrec :: Int -> a -> ShowS
  default showsPrec :: ShowRep r => Int -> a -> ShowS
  showsPrec = showsPrecDef

  show :: a -> String
  default show :: ShowRep r => a -> String
  show = showDef
  {-# MINIMAL showsPrec | show #-}

-- this is split off from Show so we can Show (Maybe# a)
class Show a => ShowList (a :: TYPE r) where
  showList :: List a -> ShowS
  default showList :: ShowListRep r => List a -> ShowS
  showList = showListDef

instance Prelude.Show a => Show (a :: Type) where
  showsPrec = Prelude.showsPrec
  show = Prelude.show

instance Prelude.Show a => ShowList (a :: Type) where
  showList = Prelude.showList

shows :: forall r (a :: TYPE r). Show a => a -> ShowS
shows = showsPrec 0

class ShowRep (r :: RuntimeRep) where
  showsPrecDef :: forall (a :: TYPE r). Show a => Int -> a -> ShowS
  showDef :: forall (a :: TYPE r). Show a => a -> String

class ListRep r => ShowListRep (r :: RuntimeRep) where
  showListDef :: forall (a :: TYPE r). ShowList a => List a -> ShowS


{-

infixr 8 **
infixl 7 /, `quot`, `rem`, `div`, `mod`

-}

class TrivialFunctorRep (r :: RuntimeRep)
instance TrivialFunctorRep (r :: RuntimeRep)

class FunctorRep f r => Functor (f :: TYPE r -> TYPE s) where
  type FunctorRep (f :: TYPE r -> TYPE s) :: RuntimeRep -> Constraint
  type FunctorRep (f :: TYPE r -> TYPE s) = (~) LiftedRep
  fmap :: forall (a :: TYPE r) rb (b :: TYPE rb). FunctorRep f rb => (a -> b) -> f a -> f # b

{-
  fmap' :: forall (a :: TYPE r) rb (b :: TYPE rb). FunctorRep f rb => (Lev a -> b) -> f a -> f # b
  (<$) :: forall (a :: TYPE r) rb (b :: TYPE rb). FunctorRep f rb => b -> f a -> f # b
  fmapConst' :: forall (a :: TYPE r) rb (b :: TYPE rb). FunctorRep f rb => Lev b -> f a -> f # b
--  rebindFunctor :: Functor f :- Functor (Rebind f r)

class Functor f => Applicative (f :: TYPE r -> TYPE s) where
  pure :: forall (a :: TYPE r). a -> f a
  pure' :: forall (a :: TYPE r). Lev a -> f a

  -- liftA2 :: forall (a :: TYPE r) rb (b :: TYPE rb). (FunctorRep f rb, FunctorRep f rc) => (a -> b -> c) -> f a -> f b -> f c
  -- liftA2 f ma mb = f <$> ma <*> mb

  (<*>) :: forall (a :: TYPE r) rb (b :: TYPE rb). (FunctorRep f LiftedRep, FunctorRep f rb) => f # (a -> b) -> f a -> f # b
  (<*) :: forall (a :: TYPE r) rb (b :: TYPE rb). FunctorRep f b => f a -> f # b -> f a
  (*>) :: forall (a :: TYPE r) rb (b :: TYPE rb). FunctorRep f b => f a -> f # b -> f # b
  m <* n = (<*>)

--  rebindApplicative :: Applicative f :- Applicative (Rebind f r)
-}


type instance Rebind Proxy r = (Proxy :: TYPE r -> Type)

instance Functor Proxy where
  type FunctorRep Proxy = TrivialFunctorRep
  fmap _ _ = Proxy

instance Functor Prelude.Maybe where
  type FunctorRep Prelude.Maybe = MaybeRep
  fmap = mapMaybe

instance (Maybe @r ~ MaybeD, MaybeRep r) => Functor (MaybeD @r) where
  type FunctorRep (MaybeD @r) = MaybeRep
  fmap = mapMaybe

-- * Printing

class PrintRep r where
  hPrint :: forall (a :: TYPE r). Show a => IO.Handle -> a -> IO ()

instance PrintRep LiftedRep where
  hPrint h x = IO.hPutStrLn h (show x)

print :: forall r (a :: TYPE r). (PrintRep r, Show a) => a -> IO ()
print = hPrint IO.stdout
