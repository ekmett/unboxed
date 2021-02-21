{-# Language UnboxedSums #-}
{-# Language KindSignatures #-}
{-# Language PatternSynonyms #-}
{-# Language MagicHash #-}
{-# Language RankNTypes #-}

module L 
  ( pattern Nothing#
  , pattern Just#
  , pattern Nil
  , pattern (:#)
  ) where

import Unlifted.Internal.Maybe hiding (Maybe)
import Unlifted.Internal.List
import GHC.Types (Type)

pattern Nothing# :: forall (a :: Type). Maybe# a
pattern Nothing# = Maybe# (# (##) | #)

pattern Just# :: forall (a :: Type). a -> Maybe# a
pattern Just# a = Maybe# (# | a #)

pattern Nil :: forall (a :: Type). List a
pattern Nil = []

pattern (:#) :: forall (a :: Type). a -> List a -> List a
pattern x :# xs = x : xs
