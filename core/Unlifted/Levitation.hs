{-# Language TypeFamilies #-}
{-# Language ConstraintKinds #-}
{-# Language StandaloneKindSignatures #-}
{-# Language RankNTypes #-}
{-# Language PolyKinds #-}

module Unlifted.Levitation 
  ( Lev
  ) where

import GHC.Types (TYPE, Type)

--------------------------------------------------------------------------------
-- * Levitation by name
--------------------------------------------------------------------------------

-- | Consider
-- @
-- class Bounded a where
--   minBound, maxBound :: a
-- @
--
-- If we go to generalize it to allow unlifted runtime representations by saying
--
-- @
-- type Bounded :: forall r. TYPE r -> Constraint
-- @
--
-- Now @minBound@ and @maxBound@ are top-level terms of type @a@ of an unlifted kind.
-- GHC rightfully rejects this. However, if we use
--
-- @
-- type Bounded :: forall r. TYPE r -> Constraint
-- class Bounded a where
--   minBound, maxBound :: (()~()) => a
-- @
--
-- Now, in core, Bounded's maxBound takes an argument, which is trivally passed by the compiler
-- and `minBound`, `maxBound` are allowed to be members. It also affects types like `mempty`
-- and `pi` that are otherwise naked members in their respective typeclass.
--
-- This corresponds to computing any unlifted value in a call-by-name fashion, and passing along
-- any lifted values as they are already lifted.
--
-- Ideally, this would look like:
--
-- @
-- type Lev :: TYPE r -> Type
-- type family Lev a where
--   Lev (a :: Type) = a 
--   Lev (a :: TYPE r) = (()~()) =>  a 
-- @
--
-- However, GHC doesn't like qualified types as the result of closed type families, so we have
-- to accept the mangling of lifted types as well, and let the compiler optimize away the (()~())
-- context.

type Lev :: TYPE r -> Type
type Lev (a :: TYPE r) = ()~() => a

