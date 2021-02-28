{-# Language DataKinds #-}
{-# Language ExplicitNamespaces #-}
{-# Language PolyKinds #-}
{-# Language RankNTypes #-}
{-# Language StandaloneKindSignatures #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}
module Unlifted.Internal.Rebind 
  ( -- RebindRep
    Rebind
  , type (#)
  ) where

import GHC.Types

-- type RebindRep :: forall r s. (TYPE r -> TYPE s) -> RuntimeRep -> RuntimeRep
-- type family RebindRep (f :: TYPE r -> TYPE s) (r' :: RuntimeRep) :: RuntimeRep

type family Rebind (f :: TYPE r -> TYPE s) (r' :: RuntimeRep) :: TYPE r' -> Type -- TYPE (RebindRep f r')

type (f :: TYPE r -> TYPE s) # (a :: TYPE r') = Rebind f r' a
infix 9 #

-- TODO: allow by switching RebindRep to give back a Type, then f # x # y could be valid syntax
