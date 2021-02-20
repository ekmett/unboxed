{-# Language DataKinds #-}
{-# Language KindSignatures #-}
{-# Language MagicHash #-}
{-# Language FlexibleInstances #-}
{-# Language NoImplicitPrelude #-}
{-# Language RebindableSyntax #-}
{-# Language TypeApplications #-}
{-# Language StandaloneKindSignatures #-}
{-# Language PolyKinds #-}
{-# Language TypeFamilies #-}
{-# Language DefaultSignatures #-}
{-# Language RankNTypes #-}
{-# Language ImportQualifiedPost #-}
{-# Language UnboxedSums #-}
{-# Language UnboxedTuples #-}
{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}

module Unlifted.Internal.Class 
  ( Eq(..), EqRep(..)
  , Ord(..), OrdRep(..)
  , Num(..), NumRep(..)
  , Semigroup(..)
  , Monoid(..)
  ) where

import Data.Kind (Constraint)
import GHC.Integer
import GHC.Prim
import GHC.Types (Type, RuntimeRep(..))
import Prelude (Ordering(..), Bool(..))
import Prelude qualified
import Unlifted.Levitation

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

{-

infixr 8 **
infixl 7 /, `quot`, `rem`, `div`, `mod`

-}

{-

-- per runtimerep backpack modules:
-- data instance List# (a :: TYPE Rep) = Nil# | Cons# a (List# a)
-- newtype instance Maybe## (a :: TYPE Rep) = Maybe# (# (##) | a #)
-- data instance Maybe# (a :: TYPE Rep) = Nothing# | Just# a

type Functor :: (TYPE r -> TYPE s) -> Constraint
class Functor (f :: TYPE r -> TYPE s)
  fmap :: (a -> b) -> f a -> f b

instance Prelude.Functor f => Functor f where
  fmap = Prelude.f

-- how do we write remotely polymorphic code for this? or do we bundle it in a one-off class?
instance ListRep r => Functor (List @r) where
  fmap f xs = case uncons# xs of 

class IfRep r where
  ifThenElse :: forall (a :: TYPE r). Bool -> a -> a -> a

instance IfRep 'LiftedRep where
  ifThenElse True x _ = x
  ifThenElse False _ y = y

-}
