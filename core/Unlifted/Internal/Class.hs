{-# Language DataKinds #-}
{-# Language FlexibleInstances #-}
{-# Language NoImplicitPrelude #-}
{-# Language RebindableSyntax #-}
{-# Language StandaloneKindSignatures #-}
{-# Language PolyKinds #-}
{-# Language TypeFamilies #-}
{-# Language DefaultSignatures #-}
{-# Language RankNTypes #-}
{-# Language ImportQualifiedPost #-}
{-# Language UnboxedSums #-}
{-# OPTIONS_HADDOCK not-home #-}

module Unlifted.Internal.Class
  ( Eq(..), EqRep(..)
  , Ord(..), OrdRep(..)
  , Num(..), NumRep(..)
  , Bounded(..)
  -- , Enum(..)
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

import Data.Kind (Constraint)
import GHC.Integer
import GHC.Prim
import GHC.Types (Type, RuntimeRep(..))
import Prelude (Ordering(..), Bool(..), Int, ShowS, String, IO)
import Prelude qualified
import Unlifted.Levitation
import Unlifted.Internal.List
import System.IO qualified as IO

-- * Standard Classes

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

-- * Functors

type Functor :: (TYPE r -> TYPE s) -> Constraint
class Functor (f :: TYPE r -> TYPE s) where
  fmap :: (a -> b) -> f a -> f b

instance Prelude.Functor f => Functor (f :: Type -> Type) where
  fmap = Prelude.fmap

-- * Printing

class PrintRep r where
  hPrint :: forall (a :: TYPE r). Show a => IO.Handle -> a -> IO ()

instance PrintRep 'LiftedRep where
  hPrint h x = IO.hPutStrLn h (show x)

print :: forall r (a :: TYPE r). (PrintRep r, Show a) => a -> IO ()
print = hPrint IO.stdout
