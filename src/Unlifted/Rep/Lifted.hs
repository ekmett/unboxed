{-# Language UnboxedSums #-}
{-# Language KindSignatures #-}
{-# Language PatternSynonyms #-}
{-# Language MagicHash #-}
{-# Language RankNTypes #-}
module Unlifted.Rep.Lifted
  ( Maybe#(Nothing#, Just#)
  , pattern Nil
  , pattern (:#)
  , Maybe(Nothing, Just)
  ) where

import Unlifted.Internal.Maybe hiding (Maybe)
import Unlifted.Internal.List
import GHC.Types (Type)
import Prelude

pattern Nothing# :: forall (a :: Type). Maybe# a
pattern Nothing# = Maybe# (# (##) | #)

pattern Just# :: forall (a :: Type). a -> Maybe# a
pattern Just# a = Maybe# (# | a #)

-- {-# COMPLETE Nothing#, Just# :: Maybe# #-}

pattern Nil :: forall (a :: Type). List a
pattern Nil = []

pattern (:#) :: forall (a :: Type). a -> List a -> List a
pattern x :# xs = x : xs

-- {-# COMPLETE Nil, (:#) :: [] #-}
