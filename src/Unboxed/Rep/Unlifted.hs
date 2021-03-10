{-# Language NoImplicitPrelude #-}
{-# Language RebindableSyntax #-}
{-# Language MagicHash #-}
{-# Language KindSignatures #-}
{-# Language DataKinds #-}
{-# Language TypeSynonymInstances #-}
{-# Language UnboxedTuples #-}
{-# Language UnboxedSums #-}
{-# Language ImportQualifiedPost #-}
{-# Language UnliftedNewtypes #-}
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

import Def.Unlifted
import GHC.Prim
import GHC.Types
import Unboxed.Internal.Class

instance Eq (MutableArray# s a) where
  x == y = isTrue# (sameMutableArray# x y)
  {-# inline (==) #-}

instance Eq (SmallMutableArray# s a) where
  x == y = isTrue# (sameSmallMutableArray# x y)
  {-# inline (==) #-}

instance Eq (MutableByteArray# s) where
  x == y = isTrue# (sameMutableByteArray# x y)
  {-# inline (==) #-}

instance Eq (MutableArrayArray# s) where
  x == y = isTrue# (sameMutableArrayArray# x y)
  {-# inline (==) #-}

instance Eq (MutVar# s a) where
  x == y = isTrue# (sameMutVar# x y)
  {-# inline (==) #-}

instance Eq (MVar# s a) where
  x == y = isTrue# (sameMVar# x y)
  {-# inline (==) #-}

instance Eq (TVar# s a) where
  x == y = isTrue# (sameTVar# x y)
  {-# inline (==) #-}

instance Eq (StableName# a) where
  x == y = isTrue# (eqStableName# x y)
  {-# inline (==) #-}
