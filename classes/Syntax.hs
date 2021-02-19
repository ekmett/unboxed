{-# Language NoImplicitPrelude #-}
{-# Language RebindableSyntax #-}
{-# Language StandaloneKindSignatures #-}
{-# Language DataKinds #-}
{-# Language RankNTypes #-}
{-# Language KindSignatures #-}

module Syntax
  ( ifThenElse
  ) where

import GHC.Types (TYPE, Bool(..))
import Rep

-- for rebindable syntax, the "real" ifThenElse works over all kinds
-- but we can't have pretty numbers without this
ifThenElse :: forall (a :: TYPE Rep). Bool -> a -> a -> a
ifThenElse False a _ = a
ifThenElse True _ a = a
