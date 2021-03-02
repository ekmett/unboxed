{-# Language DataKinds #-}
{-# Language FlexibleInstances #-}
{-# Language MagicHash #-}
{-# Language MultiParamTypeClasses #-}
{-# Language NoImplicitPrelude #-}
{-# Language PolyKinds #-}
{-# Language RankNTypes #-}
{-# Language StandaloneKindSignatures #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language TypeFamilyDependencies #-}
{-# Language UnboxedTuples #-}
{-# Language UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Other 'ListRep' instances are defined via backpack
module Unboxed.Internal.List 
  ( List
  , ListD
  , ListRep(..)
  ) where

import GHC.Types (Type, TYPE, RuntimeRep(..))
import Unboxed.Internal.Levitation
import Unboxed.Internal.Maybe
import Unboxed.Internal.Rebind

type ListD :: forall r. TYPE r -> Type
data family ListD :: TYPE r -> Type

type List :: forall r. TYPE r -> Type
type family List = (c :: TYPE r -> Type) | c -> r where
  List @'LiftedRep = []
  List @r = ListD @r

type instance Rebind [] r = List @r
type instance Rebind ListD r = List @r

class ListRep r where
  cons :: forall (a :: TYPE r). a -> List a -> List a
  cons' :: forall (a :: TYPE r). Lev a -> List a -> List a
  nil :: forall (a :: TYPE r). List a
  uncons# :: forall (a :: TYPE r). List a -> Maybe# (# a, List a #)

instance ListRep 'LiftedRep where
  cons = (:)
  cons' x xs = x : xs
  nil = []
  uncons# [] = Maybe# (# (##) | #)
  uncons# (x:xs) = Maybe# (# | (# x, xs #) #)
