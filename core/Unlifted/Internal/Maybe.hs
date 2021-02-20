{-# Language ConstraintKinds #-}
{-# Language DataKinds #-}
{-# Language DefaultSignatures #-}
{-# Language FlexibleInstances #-}
{-# Language ImportQualifiedPost #-}
{-# Language KindSignatures #-}
{-# Language MagicHash #-}
{-# Language PolyKinds #-}
{-# Language RankNTypes #-}
{-# Language StandaloneKindSignatures #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language UnboxedSums #-}
{-# Language UnboxedTuples #-}
{-# Language UnliftedNewtypes #-}

module Unlifted.Internal.Maybe 
  ( MaybeRep(..)
  -- * Unlifted Maybe
  , Maybe#(..)
  , MaybeRep#(..)
  ) where

import Unlifted.Levitation

import GHC.Types
import Prelude qualified

type MaybeRep :: RuntimeRep -> Constraint
class MaybeRep r where
  -- Lifted @Maybe@ with a (possibly) unlifted argument
  type Maybe :: TYPE r -> Type
  nothing :: forall (a :: TYPE r). Maybe a
  just :: forall (a :: TYPE r). a -> Maybe a
  maybe :: forall (a :: TYPE r) r' (b :: TYPE r'). Lev b -> (a -> b) -> Maybe a -> b

instance MaybeRep 'LiftedRep where
  type Maybe = Prelude.Maybe
  nothing = Prelude.Nothing
  just = Prelude.Just
  maybe _ j (Prelude.Just a) = j a
  maybe n _ Prelude.Nothing = n

-- | Unlifted @Maybe@ with a (possibly) unlifted argument
--
-- returns an unlifted newtype wrapping an unboxed sum.
-- This has the benefit that it gives a shape that can be used at any 'RuntimeRep'.
--
-- I just can't attach pattern synonyms to it except at known 'RuntimeRep's, so it
-- is a bit noisy. The def mixin library fixes this by supplying pattern synonyms for
-- @Nothing#@ and @Just#@ at the 'RuntimeRep' in question, but they can't be used in
-- @uncons@ because the result 'RuntimeRep' is larger than the input 'RuntimeRep'.

type Maybe# :: TYPE r -> TYPE ('SumRep '[ 'TupleRep '[],r])
newtype Maybe# (a :: TYPE r) = Maybe# (# (##) | a #)

type MaybeRep# :: RuntimeRep -> Constraint
class MaybeRep# r where
  nothing# :: forall (a :: TYPE r). Lev (Maybe# a)
  just# :: forall (a :: TYPE r). a -> Maybe# a
  maybe# :: forall (a :: TYPE r) r' (b :: TYPE r'). Lev b -> (a -> b) -> Maybe# a -> b

instance MaybeRep# 'LiftedRep where
  nothing# = Maybe# (# (##) | #)
  just# a = Maybe# (# | a #)
  maybe# _ j (Maybe# (# | a #)) = j a
  maybe# n _ (Maybe# (# (##) | #)) = n
