{-# Language DataKinds #-}
{-# Language KindSignatures #-}
{-# Language MagicHash #-}
{-# Language NoImplicitPrelude #-}
{-# Language RebindableSyntax #-}
{-# Language TypeApplications #-}
{-# Language StandaloneKindSignatures #-}
{-# Language PolyKinds #-}
{-# Language DefaultSignatures #-}
{-# Language RankNTypes #-}

module Poly where

import Data.Kind (Constraint)
import Data.Ratio
import GHC.Integer
import GHC.Prim
import GHC.Types (TYPE, RuntimeRep(..), isTrue#)
import Prelude (otherwise)

infixl 6 +, -
infixl 7 *

type Num :: forall rep. TYPE rep -> Constraint
class Num (a :: TYPE rep) where
  (+),(-),(*) :: a -> a -> a
  negate, abs, signum :: a -> a
  fromInteger :: Integer -> a

  default negate :: NumRep rep => a -> a
  negate = negateDefault

  default (-) :: NumRep rep => a -> a -> a
  (-) = minusDefault
  {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}

-- compatible with default definitions
instance Prelude.Num a => Num (a :: TYPE 'LiftedRep) where
  (+) = (Prelude.+)
  (-) = (Prelude.-)
  (*) = (Prelude.*)
  negate = Prelude.negate
  abs = Prelude.abs
  signum = Prelude.signum
  fromIngeger = Prelude.fromInteger

-- default definitions, these can't be written levity polymorphically, but here we're monomorphic
class NumRep rep where
  negateDefault :: forall (a :: TYPE rep). Num a => a -> a
  minusDefault :: forall (a :: TYPE rep). Num a => a -> a -> a

instance NumRep 'IntRep where
  negateDefault a = 0 - a
  minusDefault a b = a + negate b

instance NumRep 'WordRep where
  negateDefault a = 0 - a
  minusDefault a b = a + negate b

instance NumRep 'FloatRep where
  negateDefault a = 0 - a
  minusDefault a b = a + negate b

instance Num Int# where
  (+) = (+#)
  (-) = (-#)
  (*) = (*#)
  -- negate = negateInt#
  abs n
    | isTrue# (n >=# 0) = n
    | otherwise = negate n
  signum n
    | isTrue# (n <# 0) = negate 1
    | isTrue# (n ==# 0) = 0
    | otherwise = 1
  fromInteger = integerToInt
  {-# INLINE fromInteger #-}

