{-# Language CPP #-}
{-# Language DataKinds #-}
{-# Language ImportQualifiedPost #-}
{-# Language MagicHash #-}
{-# Language NoImplicitPrelude #-}
{-# Language PatternSynonyms #-}
{-# Language PolyKinds #-}
{-# Language RankNTypes #-}
{-# Language UnboxedTuples #-}
{-# Language UnliftedNewtypes #-}
{-# Language ViewPatterns #-}

module Unboxed.Internal.Natural where

import Control.Exception (ArithException(Underflow))
import Unboxed.Internal.Class
import Unboxed.Internal.BigNat
import GHC.Integer
import GHC.Num.Primitives
import GHC.Num.Integer
import GHC.Num.BigNat qualified as GHC
import GHC.Prim
import GHC.Exts
import Prelude ((&&), (||), Bool(..), Ordering(..))

import Unboxed.Rep.Int ()
import Unboxed.Rep.Tuple2.Word.Unlifted ()
import Unboxed.Rep.Word ()

-- #define CONSTANT_FOLDED INLINE
-- #define CONSTANT_FOLDED NOINLINE
-- #define CONSTANT_FOLDED INLINE[0]
#define CONSTANT_FOLDED NOINLINE[0]

newtype Natural# = Natural# (# Word#, BigNat# #)

instance Eq Natural# where
  N x xs == N y ys = x == y && xs == ys
  N x xs /= N y ys = x /= y || xs /= ys

instance Ord Natural# where
  NJ xs <= NJ ys = xs <= ys
  _     <= NJ{}  = True
  NJ{}  <= _     = False
  NS# x <= NS# y = x <= y

  NJ xs >= NJ ys = xs >= ys
  _     >= NJ{}  = False
  NJ{}  >= _     = True
  NS# x >= NS# y = x >= y

  NJ xs < NJ ys = xs < ys
  _     < NJ{}  = True
  NJ{}  < _     = False
  NS# x < NS# y = x < y

  NJ xs > NJ ys = xs > ys
  _     > NJ{}  = False
  NJ{}  > _     = True
  NS# x > NS# y = x > y

  compare (NJ xs) (NJ ys) = compare xs ys
  compare _       NJ{}    = LT 
  compare NJ{}    _       = GT
  compare (NS# x) (NS# y) = compare x y

  min (NJ xs) (NJ ys) = NJ (min xs ys)
  min x NJ{} = x
  min NJ{} y = y
  min (NS# x) (NS# y) = NS# (min x y)

  max (NJ xs) (NJ ys) = NJ (max xs ys)
  max _ (NJ ys)       = NJ ys
  max (NJ xs) _       = NJ xs
  max (NS# x) (NS# y) = NS# (max x y)

isNullBigNat# :: BigNat# -> Int#
isNullBigNat# n = coerce GHC.bigNatSize# n ==# 0#

isNullBigNat :: BigNat# -> Bool
isNullBigNat n = isTrue# (isNullBigNat# n)

nullBigNat# :: Void# -> BigNat# 
nullBigNat# = coerce GHC.bigNatZero#

pattern Small :: BigNat#
pattern Small <- (isNullBigNat -> True) where
  Small = nullBigNat# void#

pattern Big :: Word#
pattern Big = 18446744073709551615##

pattern N :: Word# -> BigNat# -> Natural#
pattern N w ws = Natural# (# w, ws #)

{-# complete N :: Natural# #-}

pattern NS :: Word# -> Natural# 
pattern NS w = N w Small

pattern NJ :: BigNat# -> Natural#
pattern NJ ws = N Big ws

{-# complete NJ, NS :: Natural# #-}

-- overlaps the pattern for NJ, so only use _after_ NJ.
-- or when we know 'w' isnt Big
pattern NS# :: Word# -> Natural# 
pattern NS# w <- N w _ where
  NS# w = N w (nullBigNat# void#)

{-# complete NJ, NS# :: Natural# #-}

isValidNatural# :: Natural# -> Int#
isValidNatural# (N w ws) = case coerce GHC.bigNatSize# ws of
  0# -> neWord# Big w
  1# -> bigNatCheck# ws &&# eqWord# Big (bigNatToWord# ws)
  _  -> bigNatCheck# ws

isValidNatural :: Natural# -> Bool
isValidNatural n = isTrue# (isValidNatural# n)

signumNatural# :: Natural# -> Natural#
signumNatural# (NS# 0##) = NS 0##
signumNatural# _ = NS 1##
{-# CONSTANT_FOLDED signumNatural# #-}

negateNatural# :: Natural# -> Natural#
negateNatural# (NS# 0##) = NS 0##
negateNatural# _ = raise# Underflow
{-# CONSTANT_FOLDED negateNatural# #-}

data BigNat = BigNat { getBigNat :: BigNat# }

big :: BigNat
big = BigNat (bigNatFromWord# Big)
{-# NOINLINE big #-}

-- | 'Natural' Addition
addNatural# :: Natural# -> Natural# -> Natural#
addNatural# (NS# 0##) y = y
addNatural# x (NS# 0##) = x
addNatural# (NJ xs) (NJ ys)  = NJ (xs + ys)
addNatural# (NJ xs) (NS# y) = NJ (coerce bigNatAddWord# xs y)
addNatural# (NS# x) (NJ ys) = NJ (coerce bigNatAddWord# ys x)
addNatural# (NS# x) (NS# y) = case plusWord2# x y of
  (# 0##, Big #) -> NJ (getBigNat big)
  (# 0##, l #) -> NS l
  (# h, l #) -> NJ (bigNatFromWord2# h l)
{-# CONSTANT_FOLDED addNatural# #-}

-- | 'Natural' multiplication
mulNatural# :: Natural# -> Natural# -> Natural#
mulNatural# _ (NS# 0##) = NS# 0##
mulNatural# (NS# 0##) _ = NS# 0##
mulNatural# x (NS# 1##) = x
mulNatural# (NS# 1##) y = y
mulNatural# (NJ xs) (NJ ys) = NJ (xs * ys)
mulNatural# (NJ xs) (NS# y) = NJ (coerce bigNatMulWord# xs y)
mulNatural# (NS# x) (NJ ys) = NJ (coerce bigNatMulWord# ys x)
mulNatural# (NS# x) (NS# y) = case timesWord2# x y of
  (# 0##, Big #) -> NJ (getBigNat big)
  (# 0##, xy  #) -> NS xy
  (# h  , l   #) -> NJ (bigNatFromWord2# h l)
{-# CONSTANT_FOLDED mulNatural# #-}

-- | 'Natural' subtraction. May @'Control.Exception.throw'
-- 'Control.Exception.Underflow'@.
subNatural# :: Natural# -> Natural# -> Natural#
subNatural# x (NS# 0##) = x
subNatural# (NJ xs) (NJ ys) = naturalFromBigNat# (xs - ys)
subNatural# (NJ xs) (NS# y) = naturalFromBigNat# (bigNatSubWordUnsafe# xs y)
subNatural# _       NJ{}    = raise# Underflow
subNatural# (NS# x) (NS# y) = case subWordC# x y of
  (# l, 0# #) -> NS l
  _ -> raise# Underflow
{-# CONSTANT_FOLDED subNatural# #-}

naturalFromInteger# :: Integer -> Natural#
naturalFromInteger# (IS i#)
  | i# >= 0# = NS# (int2Word# i#)
naturalFromInteger# (IP bn) = naturalFromBigNat# (BigNat# bn)
naturalFromInteger# _ = raise# Underflow
{-# CONSTANT_FOLDED naturalFromInteger# #-}

naturalToInteger# :: Natural# -> Integer
naturalToInteger# (NJ (BigNat# bn)) = IP bn
naturalToInteger# (NS# w) = wordToInteger w
{-# CONSTANT_FOLDED naturalToInteger# #-}

-- | Convert 'BigNat' to 'Natural'.
naturalFromBigNat# :: BigNat# -> Natural#
naturalFromBigNat# bn = case bigNatSize# bn of
  0# -> NS# 0##
  1# -> case bigNatToWord# bn of
    Big -> NJ bn
    w# -> NS# w#
  _ -> NJ bn
{-# CONSTANT_FOLDED naturalFromBigNat# #-}

instance Num Natural# where
  (+) = addNatural#
  {-# inline (+) #-}
  (-) = subNatural#
  {-# inline (-) #-}
  (*) = mulNatural#
  {-# inline (*) #-}
  negate = negateNatural#
  {-# inline negate #-}
  abs x = x
  {-# inline abs #-}
  signum = signumNatural#
  {-# inline signum #-}
  fromInteger = naturalFromInteger#
  {-# inline fromInteger #-}

instance Show Natural# where
  showsPrec d (NJ ws) = showsPrec d ws
  showsPrec d (NS# w) = showsPrec d w
