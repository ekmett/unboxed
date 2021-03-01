{-# Language ConstraintKinds #-}
{-# Language DataKinds #-}
{-# Language FlexibleInstances #-}
{-# Language ImportQualifiedPost #-}
{-# Language MagicHash #-}
{-# Language MultiParamTypeClasses #-}
{-# Language PolyKinds #-}
{-# Language RankNTypes #-}
{-# Language StandaloneKindSignatures #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language TypeFamilyDependencies #-}
{-# Language UnboxedSums #-}
{-# Language UnboxedTuples #-}
{-# Language UndecidableInstances #-}
{-# Language UnliftedNewtypes #-}

module Unlifted.Internal.Maybe
  ( -- MaybeFam
    Maybe
  , MaybeD
  , MaybeRep(..)
  -- * Unlifted Maybe
  , Maybe#(..)
  , MaybeRep#(..)
  ) where

import Unlifted.Levitation
import Unlifted.Internal.Rebind

import GHC.Types
import Prelude qualified

-- type instance RebindRep Prelude.Maybe r = 'LiftedRep
type instance Rebind Prelude.Maybe r' = Maybe @r'
type instance Rebind (MaybeD @r) r' = Maybe @r'

type Maybe :: forall r. TYPE r -> Type
type family Maybe = (c :: TYPE r -> Type) | c -> r where
  Maybe @'LiftedRep = Prelude.Maybe
  Maybe @r = MaybeD @r

type MaybeD :: forall r. TYPE r -> Type
data family MaybeD :: TYPE r -> Type

type MaybeRep :: RuntimeRep -> Constraint
class MaybeRep r where
  nothing :: forall (a :: TYPE r). Maybe a
  just :: forall (a :: TYPE r). a -> Maybe a
  just' :: forall (a :: TYPE r). Lev a -> Maybe a
  maybe :: forall (a :: TYPE r) r' (b :: TYPE r'). Lev b -> (a -> b) -> Maybe a -> b
  mapMaybe :: forall (a :: TYPE r) r' (b :: TYPE r'). MaybeRep r' => (a -> b) -> Maybe @r a -> Maybe @r' b

instance MaybeRep 'LiftedRep where
  nothing = Prelude.Nothing
  just = Prelude.Just
  just' a = Prelude.Just a
  maybe _ j (Prelude.Just a) = j a
  maybe n _ Prelude.Nothing = n
  mapMaybe f (Prelude.Just a) = just' (f a)
  mapMaybe _ Prelude.Nothing = nothing

-- | Unlifted @Maybe@ with a (possibly) unlifted argument

type Maybe# :: TYPE r -> TYPE ('SumRep '[ 'TupleRep '[],r])
newtype Maybe# (a :: TYPE r) = Maybe# (# (##) | a #)

type MaybeRep# :: RuntimeRep -> Constraint
class MaybeRep# r where
  nothing# :: forall (a :: TYPE r). Lev (Maybe# a)
  just# :: forall (a :: TYPE r). a -> Maybe# a
  just'# :: forall (a :: TYPE r). Lev a -> Maybe# a
  maybe# :: forall (a :: TYPE r) r' (b :: TYPE r'). Lev b -> (a -> b) -> Maybe# a -> b
  mapMaybe# :: forall (a :: TYPE r) r' (b :: TYPE r'). MaybeRep# r' => (a -> b) -> Maybe# a -> Maybe# b

instance MaybeRep# 'LiftedRep where
  nothing# = Maybe# (# (##) | #)
  just# a = Maybe# (# | a #)
  just'# a = Maybe# (# | a #)
  maybe# _ j (Maybe# (# | a #)) = j a
  maybe# n _ (Maybe# (# (##) | #)) = n
  mapMaybe# _ (Maybe# (# (##) | #)) = nothing#
  mapMaybe# f (Maybe# (# | a #)) = just'# (f a)
