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

import GHC.Types (Type, TYPE, RuntimeRep(..), Levity(..))
import Unboxed.Internal.Levitation
import Unboxed.Internal.Maybe
import Unboxed.Internal.Rebind

type ListD :: forall r. TYPE r -> Type
data family ListD :: TYPE r -> Type

type List :: forall r. TYPE r -> Type
type family List = (c :: TYPE r -> Type) | c -> r where
  List @('BoxedRep 'Lifted) = []
  List @r = ListD @r

type instance Rebind [] r = List @r
type instance Rebind ListD r = List @r

class ListRep r where
  cons :: forall (a :: TYPE r). a -> List a -> List a
  cons' :: forall (a :: TYPE r). Lev a -> List a -> List a
  nil :: forall (a :: TYPE r). List a
  uncons# :: forall (a :: TYPE r). List a -> Maybe# (# a, List a #)
  -- foldr :: forall (a :: TYPE r) rb (b :: TYPE rb). (a -> Lev b -> b) -> Lev b -> List a -> b
  -- mapList :: forall (a :: TYPE r) rb (b :: TYPE rb). ListRep rb => (a -> b) -> List a -> List b

instance ListRep ('BoxedRep 'Lifted) where
  cons = (:)
  cons' x xs = x : xs
  nil = []
  uncons# [] = Maybe# (# (##) | #)
  uncons# (x:xs) = Maybe# (# | (# x, xs #) #)
  -- foldr f z (x:xs) = f x (foldr f z xs)
  -- foldr _ z [] = z
