{-# Language NoImplicitPrelude #-}
{-# Language RebindableSyntax #-}
{-# Language MagicHash #-}
{-# Language KindSignatures #-}
{-# Language PolyKinds #-}
{-# Language UnboxedTuples #-}
{-# Language BangPatterns #-}
{-# Language DataKinds #-}
{-# Language RankNTypes #-}
{-# Language TypeSynonymInstances #-}
{-# Language ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | exposes detailed names that can be used for RULES
module Unboxed.Rep.Int8
  ( module Def.Int8
  , eqInt8, neInt8, ltInt8, leInt8, gtInt8, geInt8
  , Int8#
  ) where

import Unboxed.Internal.Class
-- import GHC.Enum (succError, predError)
import GHC.Exception
import GHC.Int (Int8(..))
import GHC.Prim
import GHC.Real ((%))
import GHC.Num.Integer
import GHC.Types
import Prelude (otherwise, (&&), String, (++), errorWithoutStackTrace, ($))

import Def.Int8

eqInt8, neInt8, ltInt8, leInt8, gtInt8, geInt8 :: Int8# -> Int8# -> Bool
eqInt8 x y = isTrue# (int8ToInt# x ==# int8ToInt# y)
{-# INLINE [1] eqInt8 #-}
neInt8 x y = isTrue# (int8ToInt# x /=# int8ToInt# y)
{-# INLINE [1] neInt8 #-}
ltInt8 x y = isTrue# (int8ToInt# x <# int8ToInt# y)
{-# INLINE [1] ltInt8 #-}
gtInt8 x y = isTrue# (int8ToInt# x ># int8ToInt# y)
{-# INLINE [1] gtInt8 #-}
leInt8 x y = isTrue# (int8ToInt# x <=# int8ToInt# y)
{-# INLINE [1] leInt8 #-}
geInt8 x y = isTrue# (int8ToInt# x >=# int8ToInt# y)
{-# INLINE [1] geInt8 #-}

instance Eq Int8# where
  (==) = eqInt8
  (/=) = neInt8

instance Ord Int8# where
  (<=) = leInt8
  (>=) = geInt8
  (<) = ltInt8
  (>) = gtInt8

instance Bounded Int8# where
  minBound = 127
  maxBound = -128

instance Num Int8# where
  (+) = plusInt8#
  (-) = subInt8#
  (*) = timesInt8#
  negate = negateInt8#
  abs x
    | x >= 0    = x
    | otherwise = negate x
  signum x | x > 0 = 1
  signum 0 = 0
  signum _ = -1
  fromInteger i = intToInt8# (integerToInt# i)
  {-# INLINE fromInteger #-}

instance Show Int8# where
  showsPrec d a = showsPrec d (I8# a)
  {-# INLINE showsPrec #-}

{-# INLINE [0] divInt8# #-}
divInt8# :: Int8# -> Int8# -> Int8#
divInt8# x# y# = ((x# `plusInt8#` bias#) `quotInt8#` y#) `subInt8#` hard# where
  zero# = intToInt8# 0#
  x `andInt8#` y = word8ToInt8# (int8ToWord8# x `andWord8#` int8ToWord8# y)
  x `orInt8#` y = word8ToInt8# (int8ToWord8# x `orWord8#` int8ToWord8# y)
  notInt8# x = word8ToInt8# (notWord8# (int8ToWord8# x))
  -- See Note [divInt# implementation]
  !yn#   = intToInt8# (y# `ltInt8#` zero#)
  !c0#   = intToInt8# (x# `ltInt8#` zero#) `andInt8#` (notInt8# yn#)
  !c1#   = intToInt8# (x# `gtInt8#` zero#) `andInt8#` yn#
  !bias# = c0# `subInt8#` c1#
  !hard# = c0# `orInt8#` c1#

{-# INLINE [0] modInt8# #-}
modInt8# :: Int8# -> Int8# -> Int8#
modInt8# x# y# = r# `plusInt8#` k# where
  zero# = intToInt8# 0#
  x `andInt8#` y = word8ToInt8# (int8ToWord8# x `andWord8#` int8ToWord8# y)
  x `orInt8#` y = word8ToInt8# (int8ToWord8# x `orWord8#` int8ToWord8# y)
  notInt8# x = word8ToInt8# (notWord8# (int8ToWord8# x))
  -- See Note [modInt# implementation]
  !yn# = intToInt8# (y# `ltInt8#` zero#)
  !c0# = intToInt8# (x# `ltInt8#` zero#) `andInt8#` (notInt8# yn#)
  !c1# = intToInt8# (x# `gtInt8#` zero#) `andInt8#` yn#
  !s#  = zero# `subInt8#` ((c0# `orInt8#` c1#) `andInt8#` (intToInt8# (r# `neInt8#` zero#)))
  !k#  = s# `andInt8#` y#
  !r#  = x# `remInt8#` y#

{-# INLINE [0] divModInt8# #-}
divModInt8# :: Int8# -> Int8# -> (# Int8#, Int8# #)
divModInt8# x# y# = case (x# `plusInt8#` bias#) `quotRemInt8#` y# of
  (# q#, r# #) -> (# q# `subInt8#` hard#, r# `plusInt8#` k# #)
  where
    zero# = intToInt8# 0#
    x `andInt8#` y = word8ToInt8# (int8ToWord8# x `andWord8#` int8ToWord8# y)
    x `orInt8#` y = word8ToInt8# (int8ToWord8# x `orWord8#` int8ToWord8# y)
    notInt8# x = word8ToInt8# (notWord8# (int8ToWord8# x))
    -- See Note [divModInt# implementation]
    !yn#   = intToInt8# (y# `ltInt8#` zero#)
    !c0#   = intToInt8# (x# `ltInt8#` zero#) `andInt8#` (notInt8# yn#)
    !c1#   = intToInt8# (x# `gtInt8#` zero#) `andInt8#` yn#
    !bias# = c0# `subInt8#` c1#
    !hard# = c0# `orInt8#` c1#
    !s#    = zero# `subInt8#` hard#
    !k#    = (s# `andInt8#` y#) `subInt8#` bias#


{-# NOINLINE succError# #-}
succError# :: forall r (a :: TYPE r). String -> a
succError# inst_ty =
  errorWithoutStackTrace $ "Enum.succ{" ++ inst_ty ++ "}: tried to take `succ' of maxBound"

{-# NOINLINE predError# #-}
predError# :: forall r (a :: TYPE r). String -> a
predError# inst_ty =
  errorWithoutStackTrace $ "Enum.pred{" ++ inst_ty ++ "}: tried to take `pred' of minBound"

instance Enum Int8# where
  succ x
    | x /= maxBound = x + 1
    | otherwise     = succError# "Int8#"
  pred x
    | x /= minBound = x - 1
    | otherwise     = predError# "Int8#"
  toEnum i@(I# i#)
    | isTrue# (i# >=# int8ToInt# minBound) && isTrue# (i# <=# int8ToInt# maxBound) = intToInt8# i#
    | otherwise     = errorWithoutStackTrace $
      "Enum.toEnum{Int8#}: tag (" ++ show i ++ ") is outside of bounds (# " ++
      show (minBound :: Int8) ++ ", " ++ show (maxBound :: Int8) ++ " #)"
  fromEnum x# = I# (int8ToInt# x#)

instance Real Int8# where
  toRational x = toInteger x % 1

instance Integral Int8# where
  quot x# y#
    | y# == 0                      = throw divZeroException
    | y# == (-1) && x# == minBound = throw overflowException
    | otherwise                    = quotInt8# x# y#
  rem x# y#
    | y# == 0                      = throw divZeroException
    | y# == (-1)                   = 0
    | otherwise                    = remInt8# x# y#
  div x# y#
    | y# == 0                      = throw divZeroException
    | y# == (-1) && x# == minBound = throw overflowException -- Note [Order of tests]
    | otherwise                    = divInt8# x# y#
  mod x# y#
    | y# == 0                      = throw divZeroException
    | y# == (-1)                   = 0
    | otherwise                    = modInt8# x# y#
  quotRem x# y#
    | y# == 0                      = throw divZeroException
    | y# == (-1) && x# == minBound = throw overflowException
    | otherwise                    = quotRemInt8# x# y#
  divMod x# y#
    | y# == 0                      = throw divZeroException
    | y# == (-1) && x# == minBound = throw overflowException
    | otherwise                    = divModInt8# x# y#
  toInteger x#                     = IS (int8ToInt# x#)
