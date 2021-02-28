{-# Language NoImplicitPrelude #-}
{-# Language RebindableSyntax #-}
{-# Language MagicHash #-}
{-# Language KindSignatures #-}
{-# Language DataKinds #-}
{-# Language TypeSynonymInstances #-}
{-# Language UnboxedTuples #-}
{-# Language ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module T0 
  ( module Def.T0
  , Proxy#, proxy#
  , State#
  ) where

import Unlifted.Internal.Class
import GHC.Prim
import GHC.Types
import Prelude (showString)

import Def.T0

instance Eq (# #) where
  _ == _  = True
  _ /= _  = False
  
instance Ord (# #) where
  _ <= _ = True
  _ >= _ = True
  _ < _ = False
  _ > _ = False
  compare _ _ = EQ
  min _ _ = (# #) 
  max _ _ = (# #)

instance Bounded (# #) where
  minBound = (# #) 
  maxBound = (# #)

instance Show (# #) where
  showsPrec _ _ = showString "(# #)"

instance Eq (Proxy# a) where
  _ == _  = True
  _ /= _  = False

instance Ord (Proxy# a) where
  _ <= _ = True
  _ >= _ = True
  _ < _ = False
  _ > _ = False
  compare _ _ = EQ
  min _ _ = proxy#
  max _ _ = proxy#

instance Bounded (Proxy# a) where
  minBound = proxy#
  maxBound = proxy#


-- type instance Rebind Proxy# = Proxy#

{-
instance Functor Proxy# where
  fmap _ _ = proxy#
-}

instance Show (Proxy# a) where
  showsPrec _ _ = showString "proxy#"
