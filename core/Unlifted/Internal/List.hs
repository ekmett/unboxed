{-# Language DataKinds #-}
{-# Language MagicHash #-}
{-# Language UnboxedTuples #-}
{-# Language NoImplicitPrelude #-}
{-# Language StandaloneKindSignatures #-}
{-# Language PolyKinds #-}
{-# Language TypeFamilies #-}
{-# Language RankNTypes #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Other 'ListRep' instances are defined via backpack
module Unlifted.Internal.List 
  ( ListRep(..)
  ) where

import GHC.Types (Type, TYPE, RuntimeRep(..))
import Unlifted.Internal.Maybe

class ListRep r where
  type List :: TYPE r -> Type
  cons# :: forall (a :: TYPE r). a -> List a -> List a
  nil# :: forall (a :: TYPE r). List a
  uncons# :: forall (a :: TYPE r). List a -> Maybe# (# a, List a #)

instance ListRep 'LiftedRep where
  type List = []
  cons# = (:)
  nil# = []
  uncons# [] = Maybe# (# (##) | #)
  uncons# (x:xs) = Maybe# (# | (# x, xs #) #)
