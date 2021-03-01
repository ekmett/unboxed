{-# Language DataKinds #-}
{-# Language FlexibleInstances #-}
{-# Language FunctionalDependencies #-}
{-# Language UndecidableInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language PolyKinds #-}
{-# Language RankNTypes #-}
{-# Language StandaloneKindSignatures #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}
module Unlifted.Internal.Rebind 
  ( Bind
  , Rebind
  ) where

import GHC.Types

class Bind (f :: TYPE r -> TYPE s) (f' :: TYPE r' -> TYPE s') | f r' -> f' s'

class (Bind f f', Bind f' f) => Rebind (f :: TYPE r -> TYPE s) (f' :: TYPE r' -> TYPE s') | f r' -> f' s', f' r -> f s
instance (Bind f f', Bind f' f) => Rebind f f'

{-
type RebindRep :: forall r s. (TYPE r -> TYPE s) -> RuntimeRep -> RuntimeRep
type family RebindRep (f :: TYPE r -> TYPE s) (r' :: RuntimeRep) :: RuntimeRep

type family Rebind (f :: TYPE r -> TYPE s) (r' :: RuntimeRep) :: TYPE r' -> TYPE (RebindRep f r')

type (f :: TYPE r -> TYPE s) # (a :: TYPE r') = Rebind f r' a
infix 9 #
-}

-- TODO: allow by switching RebindRep to give back a Type, then f # x # y could be valid syntax
