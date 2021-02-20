{-# Language DataKinds #-}
{-# Language KindSignatures #-}
{-# Language NoImplicitPrelude #-}
{-# Language PolyKinds #-}
{-# Language TypeFamilies #-}
{-# Language RankNTypes #-}

module Unlifted.Internal.Syntax where

import GHC.Types (TYPE, Bool(..))
import Unlifted.Levitation

-- | Unfortunately there is no way to turn on just the numeric part of RebindableSyntax.
--
-- This kind polymorphic @if then else@ for overloaded Syntax is invisibly lazy in its arguments
--
-- >>> :m + GHC.Int
-- >>> I# (ifThenElse False undefined 1#)
-- 1
ifThenElse :: forall r (a :: TYPE r). Bool -> Lev a -> Lev a -> a
ifThenElse True a _ = a
ifThenElse False _ b = b
{-# INLINE ifThenElse #-}
