{-# Language NoImplicitPrelude #-}
{-# Language RebindableSyntax #-}
{-# Language MagicHash #-}
{-# Language KindSignatures #-}
{-# Language DataKinds #-}
{-# Language TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unboxed.Rep.Unlifted
  ( module Def.Unlifted
  , MutableArray#
  , SmallMutableArray#
  , MutableByteArray#
  , MutableArrayArray#
  , MutVar#
  , MVar#
  , TVar#
  , StableName#
  , Weak#
  ) where

import Unboxed.Internal.Class
import GHC.Prim
import GHC.Types

import Def.Unlifted

instance Eq (MutableArray# s a) where
  x == y = isTrue# (sameMutableArray# x y)

instance Eq (SmallMutableArray# s a) where
  x == y = isTrue# (sameSmallMutableArray# x y)

instance Eq (MutableByteArray# s) where
  x == y = isTrue# (sameMutableByteArray# x y)

instance Eq (MutableArrayArray# s) where
  x == y = isTrue# (sameMutableArrayArray# x y)

instance Eq (MutVar# s a) where
  x == y = isTrue# (sameMutVar# x y)

instance Eq (MVar# s a) where
  x == y = isTrue# (sameMVar# x y)

instance Eq (TVar# s a) where
  x == y = isTrue# (sameTVar# x y)

instance Eq (StableName# a) where
  x == y = isTrue# (eqStableName# x y)
