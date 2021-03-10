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

module Unboxed.Internal.BigNat where

import Control.Exception (throw, ArithException(Underflow))
import Data.Coerce
import GHC.Num.Integer
import GHC.Num.BigNat qualified as GHC
import GHC.Prim
import GHC.Types
import Unboxed.Internal.Class
import Unboxed.Rep.Unlifted ()

bigNatSize# :: BigNat# -> Int#
bigNatSize# = coerce GHC.bigNatSize#
{-# inline bigNatSize# #-}

bigNatToWord# :: BigNat# -> Word#
bigNatToWord# = coerce GHC.bigNatToWord#
{-# inline bigNatToWord# #-}

bigNatFromWord# :: Word# -> BigNat#
bigNatFromWord# = coerce GHC.bigNatFromWord#
{-# inline bigNatFromWord# #-}

bigNatFromWord2# :: Word# -> Word# -> BigNat#
bigNatFromWord2# = coerce GHC.bigNatFromWord2#
{-# inline bigNatFromWord2# #-}

bigNatAddWord# :: BigNat# -> Word# -> BigNat#
bigNatAddWord# = coerce GHC.bigNatAddWord#
{-# inline bigNatAddWord# #-}

bigNatSubWord# :: BigNat# -> Word# -> (# (##) | BigNat# #)
bigNatSubWord# = coerce GHC.bigNatSubWord#
{-# inline bigNatSubWord# #-}

bigNatSubWordUnsafe# :: BigNat# -> Word# -> BigNat#
bigNatSubWordUnsafe# = coerce GHC.bigNatSubWordUnsafe#
{-# inline bigNatSubWordUnsafe# #-}

bigNatMulWord# :: BigNat# -> Word# -> BigNat#
bigNatMulWord# = coerce GHC.bigNatMulWord#
{-# inline bigNatMulWord# #-}

bigNatCheck# :: BigNat# -> Int#
bigNatCheck# = coerce GHC.bigNatCheck#
{-# inline bigNatCheck# #-}

-- | Simple unlifted natural numbers. Used inside other numeric types.
-- Contained in an unlifted newtype unlike 'GHC.Num.BigNat.BigNat#'
-- so we can hang instances off it.
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
