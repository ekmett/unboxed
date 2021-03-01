{-# Language DataKinds #-}
{-# Language ExplicitNamespaces #-}
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
  ( Rebind
  , type (#)
  ) where

import GHC.Types

type family Rebind (f :: TYPE r -> k) (r' :: RuntimeRep) :: TYPE r' -> k

type (f :: TYPE r -> k) # (a :: TYPE r') = Rebind f r' a
infix 9 #
