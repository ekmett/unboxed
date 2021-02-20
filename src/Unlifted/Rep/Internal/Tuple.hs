{-# Language NoImplicitPrelude #-}
{-# Language RebindableSyntax #-}
{-# Language MagicHash #-}
{-# Language KindSignatures #-}
{-# Language DataKinds #-}
{-# Language TypeSynonymInstances #-}
{-# Language ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unlifted.Rep.Internal.Tuple where

import Unlifted.Internal.Class
import GHC.Prim
import GHC.Integer
import GHC.Types
import Prelude qualified

import TupleDef ()

instance Eq (# #) where
  _ == _  = True
  _ /= _  = False
  
instance Ord (# #) where
  _ <= _ = True
  _ >= _ = True
  _ < _ = False
  _ > _ = False
  compare _ _ = Eq
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
  compare _ _ = Eq
  min _ _ = proxy#
  max _ _ = proxy#

instance Bounded (Proxy# a) where
  minBound = proxy#
  maxBound = proxy#

instance Functor Proxy# where
  fmap _ _ = proxy#

instance Show (Proxy# a) where
  showsPrec _ _ = showString "proxy#"
