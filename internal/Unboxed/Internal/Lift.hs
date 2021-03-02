{-# Language ConstraintKinds #-}
{-# Language DataKinds #-}
{-# Language FlexibleInstances #-}
{-# Language ImportQualifiedPost #-}
{-# Language MagicHash #-}
{-# Language PolyKinds #-}
{-# Language RankNTypes #-}
{-# Language StandaloneKindSignatures #-}
{-# Language TypeFamilies #-}
{-# Language TypeFamilyDependencies #-}
{-# Language UnboxedSums #-}
{-# Language UnboxedTuples #-}
{-# Language UnliftedNewtypes #-}

module Unboxed.Internal.Lift
  ( LiftRep(..)
  , pureLift
  , mapLift
  , apLift
  , bindLift
  ) where

import Unboxed.Internal.Levitation
import GHC.Types
import Prelude qualified

-- explicit Lev

type LiftRep :: RuntimeRep -> Constraint
class LiftRep r where
  data Lift :: TYPE r -> Type
  -- Lifted @Maybe@ with a (possibly) unlifted argument
  lift :: forall (a :: TYPE r). a -> Lift a
  lift' :: forall (a :: TYPE r). Lev a -> Lift a
  unlift :: forall (a :: TYPE r). Lift a -> a
  applyLift :: forall (a :: TYPE r) rb (b :: TYPE rb).
    (a -> b) -> Lift a -> b

pureLift :: forall (a :: TYPE r). a -> Lift a
pureLift = lift

mapLift
  :: forall ra (a :: TYPE r) rb (b :: TYPE r).
     (LiftRep ra, LiftRep rb)
  => (a -> b) -> Lift a -> Lift b
mapLift f l = lift' (applyLift f l)

apLift :: Lift (a -> b) -> Lift a -> Lift b
apLift (Lift f) = mapLift f

bindLift
  :: forall ra (a :: TYPE r) rb (b :: TYPE r).
     LiftRep ra
  => (a -> Lift b) -> Lift a -> Lift b
bindLift = applyLift

instance LiftRep 'LiftedRep where
  newtype Lift a = Lift a
  lift = Lift
  lift' a = Lift a
  unlift (Lift a) = a
  applyLift (Lift a) f = f a

{-
class Liftable (a :: TYPE r) where
  type Lifted a :: Type
  lifted :: a -> Lifted a
  lifted' :: Lev a -> Lifted a
  unlifted :: Lifted a -> a
  applyLifted :: (a -> b) -> Lifted a -> b

instance Liftable Int# where
  type Lifted Int# = Int
  lifted = I#
  lifted' x = I# x
  unlifted (I# x) = x
  applyLifted f (I# x) = f x

newtype Lowered (a :: TYPE r) = Lowered a
-- 
instance Eq (Lifted a) => Eq (Lowered (a :: TYPE 'IntRep)) where
  x == y = lifted x == lifted y
  x /= y = lifted x /= lifted y

instance Num (Lifted a) => Num (Lowered (a :: TYPE 'IntRep)) where
  x + y = unlifted (lifted x + lifted y)

deriving via Lowered Int# instance Num Int#

newtype Foo# = Foo# Int#
  deriving (Num, Eq, Ord) via Lowered Int#
-}
