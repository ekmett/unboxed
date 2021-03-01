{-# Language UnboxedSums #-}
{-# Language KindSignatures #-}
{-# Language PatternSynonyms #-}
{-# Language MagicHash #-}
{-# Language RankNTypes #-}

module Unboxed.Rep.Lifted
  ( pattern Nothing#
  , pattern Just#
  , pattern Nil
  , pattern (:#)
  ) where

import Unboxed.Internal.Maybe hiding (Maybe)
import Unboxed.Internal.List
import GHC.Types (Type)

pattern Nothing# :: forall (a :: Type). Maybe# a
pattern Nothing# = Maybe# (# (##) | #)

pattern Just# :: forall (a :: Type). a -> Maybe# a
pattern Just# a = Maybe# (# | a #)

pattern Nil :: forall (a :: Type). List a
pattern Nil = []

pattern (:#) :: forall (a :: Type). a -> List a -> List a
pattern x :# xs = x : xs
