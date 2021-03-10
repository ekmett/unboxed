{-# Language NoImplicitPrelude #-}
{-# Language RebindableSyntax #-}
{-# Language MagicHash #-}
{-# Language KindSignatures #-}
{-# Language DataKinds #-}
{-# Language TypeSynonymInstances #-}
{-# Language UnboxedTuples #-}
{-# Language UnboxedSums #-}
{-# Language ImportQualifiedPost #-}
{-# Language UnliftedNewtypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unboxed.Rep.Unlifted
  ( module Def.Unlifted
  , MutableArray#
  , SmallMutableArray#
  , MutableByteArray#
  , MutableArrayArray#
  , MutVar#
  , MVar#
  , TVar#
  , StableName#
  , Weak#
  , BigNat#
  ) where

import Control.Exception (throw, ArithException(Underflow))
import Data.Coerce
import Def.Unlifted
import GHC.Num.Integer
import GHC.Num.BigNat qualified as GHC
import GHC.Prim
import GHC.Types
import Unboxed.Internal.Class

instance Eq (MutableArray# s a) where
  x == y = isTrue# (sameMutableArray# x y)
  {-# inline (==) #-}

instance Eq (SmallMutableArray# s a) where
  x == y = isTrue# (sameSmallMutableArray# x y)
  {-# inline (==) #-}

instance Eq (MutableByteArray# s) where
  x == y = isTrue# (sameMutableByteArray# x y)
  {-# inline (==) #-}

instance Eq (MutableArrayArray# s) where
  x == y = isTrue# (sameMutableArrayArray# x y)
  {-# inline (==) #-}

instance Eq (MutVar# s a) where
  x == y = isTrue# (sameMutVar# x y)
  {-# inline (==) #-}

instance Eq (MVar# s a) where
  x == y = isTrue# (sameMVar# x y)
  {-# inline (==) #-}

instance Eq (TVar# s a) where
  x == y = isTrue# (sameTVar# x y)
  {-# inline (==) #-}

instance Eq (StableName# a) where
  x == y = isTrue# (eqStableName# x y)
  {-# inline (==) #-}

newtype BigNat# = BigNat# GHC.BigNat#

instance Eq BigNat# where
  (==) = coerce GHC.bigNatEq
  {-# inline (==) #-}
  (/=) = coerce GHC.bigNatNe
  {-# inline (/=) #-}

instance Ord BigNat# where
  (<=) = coerce GHC.bigNatLe
  {-# inline (<=) #-}
  (>=) = coerce GHC.bigNatGe
  {-# inline (>=) #-}
  (<) = coerce GHC.bigNatLt
  {-# inline (<) #-}
  (>) = coerce GHC.bigNatGt
  {-# inline (>) #-}
  compare = coerce GHC.bigNatCompare
  {-# inline compare #-}

instance Show BigNat# where
  showsPrec d x = showsPrec d (toInteger x)

instance Num BigNat# where
  (+) = coerce GHC.bigNatAdd
  {-# inline (+) #-}

  x - y = case coerce GHC.bigNatSub x y of
    (# (##) | #) -> throw Underflow
    (# | z #) -> z
  {-# inline (-) #-}

  (*) = coerce GHC.bigNatMul
  {-# inline (*) #-}

  negate x
    | isTrue# (coerce GHC.bigNatEqWord# x 0##) = x
    | True = throw Underflow
  {-# inline negate #-}

  signum x
    | isTrue# (coerce GHC.bigNatEqWord# x 0##) = x
    | True = coerce GHC.bigNatOne# void#
  {-# inline signum #-}

  abs x = x
  {-# inline abs #-}

  fromInteger (IS i)
    | isTrue# (i <# 0#) = throw Underflow
    | True              = coerce GHC.bigNatFromAbsInt# i
  fromInteger IN{} = throw Underflow
  fromInteger (IP n) = BigNat# n
  {-# inline fromInteger #-}

instance Enum BigNat# where
  fromEnum = coerce GHC.bigNatToInt
  {-# inline fromEnum #-}

  toEnum (I# i)
    | isTrue# (i <# 0#) = throw Underflow
    | True              = coerce GHC.bigNatFromAbsInt# i
  {-# inline toEnum #-}

  pred x = case coerce GHC.bigNatSubWord# x 1## of
    (# (##) | #) -> throw Underflow
    (# | z #) -> z
  {-# inline pred #-}

  succ x = coerce GHC.bigNatAddWord# x 1##
  {-# inline succ #-}

  -- enumFromThenTo...

instance Real BigNat# where
  toRational x = toRational (toInteger x)
  {-# inline toRational #-}

instance Integral BigNat# where
  quot = coerce GHC.bigNatQuot
  {-# inline quot #-}

  rem = coerce GHC.bigNatRem
  {-# inline rem #-}

  div = coerce GHC.bigNatQuot
  {-# inline div #-}

  mod = coerce GHC.bigNatRem
  {-# inline mod #-}

  quotRem = coerce GHC.bigNatQuotRem#
  {-# inline quotRem #-}

  divMod = coerce GHC.bigNatQuotRem#
  {-# inline divMod #-}

  toInteger (BigNat# bn)
    | isTrue# ((coerce GHC.bigNatSize# bn ==# 1#) `andI#` (i# >=# 0#)) = IS i#
    | True = IP bn
    where i# = coerce GHC.bigNatToInt# bn
  {-# inline toInteger #-}
