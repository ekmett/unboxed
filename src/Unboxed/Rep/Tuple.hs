{-# Language DataKinds #-}
{-# Language FlexibleInstances #-}
{-# Language ImportQualifiedPost #-}
{-# Language KindSignatures #-}
{-# Language MagicHash #-}
{-# Language NoImplicitPrelude #-}
{-# Language PolyKinds #-}
{-# Language RebindableSyntax #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language TypeSynonymInstances #-}
{-# Language UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unboxed.Rep.Tuple
  ( module Tuple
  -- * Proxy#
  , Proxy#
  , proxy#
  , ProxyRep#
  -- * State#
  , State#
  ) where

import Unboxed.Internal.Class
import Unboxed.Internal.Rebind
import GHC.Prim
import GHC.Types
import Prelude (showString)

import Tuple

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

class ProxyRep# (r :: RuntimeRep)
instance ProxyRep# r

type instance Rebind Proxy# r = Proxy# @(TYPE r)

instance Functor Proxy# where
  type FunctorRep Proxy# = ProxyRep#
  fmap _ _ = proxy#

instance Show (Proxy# a) where
  showsPrec _ _ = showString "proxy#"
