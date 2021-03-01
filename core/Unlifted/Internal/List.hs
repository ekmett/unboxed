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
module Unlifted.Internal.List 
  ( List
  , ListFam
  , ListRep(..)
  ) where

import GHC.Types (Type, TYPE, RuntimeRep(..))
import Unlifted.Internal.Maybe
import Unlifted.Internal.Rebind

type List :: forall r. TYPE r -> Type
type family List = (c :: TYPE r -> Type) | c -> r where
  List @'LiftedRep = []
  List @r = ListFam @r

instance (f' ~ List @r') => Bind [] (f' :: TYPE r' -> Type) 
instance (f' ~ List @r') => Bind ListFam (f' :: TYPE r' -> Type)

type ListFam :: forall r. TYPE r -> Type
data family ListFam :: TYPE r -> Type

class ListRep r where
  cons :: forall (a :: TYPE r). a -> List a -> List a
  nil :: forall (a :: TYPE r). List a
  uncons# :: forall (a :: TYPE r). List a -> Maybe# (# a, List a #)

instance ListRep 'LiftedRep where
  cons = (:)
  nil = []
  uncons# [] = Maybe# (# (##) | #)
  uncons# (x:xs) = Maybe# (# | (# x, xs #) #)

{-
type instance RebindRep [] r' = 'LiftedRep
type instance RebindRep ListFam r' = 'LiftedRep
type instance Rebind [] r' = List @r'
type instance Rebind ListFam r' = List @r'
-}
