{-# Language NoImplicitPrelude #-}
{-# Language RebindableSyntax #-}
{-# Language MagicHash #-}
{-# Language KindSignatures #-}
{-# Language DataKinds #-}
{-# Language TypeSynonymInstances #-}
{-# Language UnboxedTuples #-}
{-# Language UnboxedSums #-}
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
import Unboxed.Internal.Class
import GHC.Num.Integer
import GHC.Num.BigNat
import GHC.Prim
import GHC.Types

import Def.Unlifted

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

instance Eq BigNat# where
  (==) = bigNatEq
  {-# inline (==) #-}
  (/=) = bigNatNe
  {-# inline (/=) #-}

instance Ord BigNat# where
  (<=) = bigNatLe
  {-# inline (<=) #-}
  (>=) = bigNatGe
  {-# inline (>=) #-}
  (<) = bigNatLt
  {-# inline (<) #-}
  (>) = bigNatGt
  {-# inline (>) #-}
  compare = bigNatCompare
  {-# inline compare #-}

instance Show BigNat# where
  showsPrec d x = showsPrec d (toInteger x)

instance Num BigNat# where
  (+) = bigNatAdd
  {-# inline (+) #-}

  x - y = case bigNatSub x y of
    (# (##) | #) -> throw Underflow
    (# | z #) -> z
  {-# inline (-) #-}

  (*) = bigNatMul
  {-# inline (*) #-}

  negate x
    | isTrue# (bigNatEqWord# x 0##) = x
    | True = throw Underflow
  {-# inline negate #-}

  signum x
    | isTrue# (bigNatEqWord# x 0##) = x
    | True = bigNatOne# void#
  {-# inline signum #-}

  abs x = x
  {-# inline abs #-}

  fromInteger (IS i)
    | isTrue# (i <# 0#) = throw Underflow
    | True              = bigNatFromAbsInt# i
  fromInteger IN{} = throw Underflow
  fromInteger (IP n) = n
  {-# inline fromInteger #-}

instance Enum BigNat# where
  fromEnum = bigNatToInt
  {-# inline fromEnum #-}

  toEnum (I# i)
    | isTrue# (i <# 0#) = throw Underflow
    | True              = bigNatFromAbsInt# i
  {-# inline toEnum #-}

  pred x = case bigNatSubWord# x 1## of
    (# (##) | #) -> throw Underflow
    (# | z #) -> z
  {-# inline pred #-}

  succ x = bigNatAddWord# x 1##
  {-# inline succ #-}

  -- enumFromThenTo...

instance Real BigNat# where
  toRational x = toRational (toInteger x)
  {-# inline toRational #-}

instance Integral BigNat# where
  quot = bigNatQuot
  {-# inline quot #-}

  rem = bigNatRem
  {-# inline rem #-}

  div = bigNatQuot
  {-# inline div #-}

  mod = bigNatRem
  {-# inline mod #-}

  quotRem = bigNatQuotRem#
  {-# inline quotRem #-}

  divMod = bigNatQuotRem#
  {-# inline divMod #-}

  toInteger bn
    | isTrue# ((bigNatSize# bn ==# 1#) `andI#` (i# >=# 0#)) = IS i#
    | True = IP bn
    where i# = bigNatToInt# bn
  {-# inline toInteger #-}
