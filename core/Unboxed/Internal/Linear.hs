{-# Language LinearTypes #-}
{-# Language StandaloneKindSignatures #-}
{-# Language RoleAnnotations #-}
{-# Language MagicHash #-}
{-# Language TypeFamilies #-}
{-# Language UnboxedTuples #-}
{-# Language PolyKinds #-}
{-# Language DataKinds #-}
{-# Language GADTs #-}
{-# Language UnliftedNewtypes #-}
{-# Language MultiParamTypeClasses #-}
{-# Language BlockArguments #-}
{-# Language FlexibleInstances #-}
{-# Language ScopedTypeVariables #-}
{-# Language PatternSynonyms #-}
{-# Language RankNTypes #-}
{-# Language TypeApplications #-}
{-# Language InstanceSigs #-}
{-# Language ImportQualifiedPost #-}
{-# Language FunctionalDependencies #-}

module Unboxed.Internal.Linear 
  ( Consumable(..)
  , Ur(..)
  , UrRep(..)
  , urapp
  , toLinear, toLinear2, toLinear3
  , ST(..)
  , STRep(..)
  ) where

import Data.Unrestricted.Linear qualified as Linear (Consumable(..))
import GHC.Exts
-- import GHC.Prim
import GHC.Types
import Unsafe.Coerce
import Unboxed.Internal.Levitation
import Unboxed.Internal.Linear.Ur
-- import Unboxed.Internal.Maybe as Maybe
-- import Prelude qualified

type Consumable :: TYPE r -> Constraint
class Consumable  (a :: TYPE r) where
  consume :: a %1 -> ()
instance Linear.Consumable a => Consumable (a :: Type) where
  consume = Linear.consume

type UrRep :: RuntimeRep -> Constraint
class UrRep r where
  pureUr :: forall (a :: TYPE r). a -> Ur a
  extractUr :: forall (a :: TYPE r). Ur a -> a
  bindUr :: forall (a :: TYPE r) rb (b :: TYPE rb). Ur a %1 -> (a %1 -> Ur b) %1 -> Ur b
  mapUr :: forall (a :: TYPE r) rb (b :: TYPE rb). (a -> b) %1 -> Ur a %1 -> Ur b

instance UrRep 'LiftedRep where
  pureUr = Ur
  extractUr (Ur a) = a
  bindUr (Ur a) f = f a
  mapUr = toLinear2 coerce

{-
instance UrRep 'UnboxedRep where
  pureUr = Ur
  unUr (Ur a) = a
  bindUr (Ur a) f = f a
  mapUr = coerce

instance UrRep 'IntRep where
  pureUr = Ur
  unUr (Ur a) = a
  bindUr (Ur a) f = f a
  mapUr = coerce
-}

urapp :: forall r (a :: TYPE r) s (b :: TYPE s). (a -> b) %1 -> Ur a %1 -> b
urapp = unsafeCoerce unsafeCoerce

toLinear :: forall ra (a :: TYPE ra) rb (b :: TYPE rb). (a -> b) %1 -> a %1 -> b
toLinear = unsafeCoerce unsafeCoerce

toLinear2 :: forall ra (a :: TYPE ra) rb (b :: TYPE rb) rc (c :: TYPE rc). 
  (a -> b -> c) %1 -> a %1 -> b %1 -> c
toLinear2 = unsafeCoerce unsafeCoerce

toLinear3 :: forall ra (a :: TYPE ra) rb (b :: TYPE rb) rc (c :: TYPE rc) rd (d :: TYPE rd).
  (a -> b -> c -> d) %1 -> a %1 -> b %1 -> c %1 -> d
toLinear3 = unsafeCoerce unsafeCoerce

type ST :: Type -> forall r. TYPE r -> Type
type role ST nominal representational
newtype ST s a where
  ST :: forall r s (a :: TYPE r). { unST :: State# s %1 -> (# State# s, a #) } %1 -> ST s a

type P :: forall r. TYPE r -> Type
newtype P a = P (forall s. ST s a)

class STRep r where
  runST :: forall (a :: TYPE r). (forall s. ST s a) %1 -> a
  bindST :: forall (a :: TYPE r) rb (b :: TYPE rb) s. ST s a %1 -> (a %1 -> ST s b) %1 -> ST s b
  pureST :: forall (a :: TYPE r) s. a %1 -> ST s a
  -- dataFmapST    :: forall (a :: TYPE r) rb (b :: TYPE rb). STRep rb => (a %1 -> b)    -> ST s a %1 -> ST s b
  fmapST :: forall (a :: TYPE r) rb (b :: TYPE rb) s. STRep rb => (a %1 -> b) %1 -> ST s a %1 -> ST s b
  mkSTRes :: forall (b :: TYPE r) s. State# s -> Lev b -> (# State# s, b #)
--  app :: forall (a :: TYPE r) rb (b :: TYPE rb). (a -> b) -> a -> b

{-
class FunRep r where
  (.) :: forall (a :: TYPE r) rb (b :: TYPE rb) rc (c :: TYPE rc). (Lev b -> c) -> (a -> b) -> a -> c

instance FunRep 'LiftedRep where
  (.) f g a = f (g a)
-}

instance STRep 'LiftedRep where
  runST :: forall (a :: Type). (forall s. ST s a) %1 -> a
  runST m = toLinear go (P m) where
    go :: P a -> a
    go (P (ST f)) = runRW# \s -> case f s of
      (# _, a #) -> a

  pureST a = ST \s -> (# s, a #)

  bindST :: forall (a :: TYPE 'LiftedRep) rb (b :: TYPE rb) s. ST s a %1 -> (a %1 -> ST s b) %1 -> ST s b
  bindST f0 k0 = ST (\s -> toLinear3 go f0 k0 s) where
    go :: ST s a -> (a %1 -> ST s b) -> State# s -> (# State# s, b #)
    go f k s = case unST f s of
      (# s', a #) -> unST (k a) s'

  fmapST :: forall (a :: TYPE 'LiftedRep) rb (b :: TYPE rb) s. STRep rb => (a %1 -> b) %1 -> ST s a %1 -> ST s b
  fmapST f0 m0 = ST (\s -> toLinear3 go f0 m0 s) where
    go :: (a %1 -> b) -> ST s a -> State# s -> (# State# s, b #)
    go f m s = case unST m s of
      (# s', a #) -> mkSTRes s' (f a)

  mkSTRes :: forall (b :: TYPE 'LiftedRep) s. State# s -> Lev b -> (# State# s, b #)
  mkSTRes s a = (# s , a #)

-- class Functor (f :: TYPE r -> TYPE s) where

{-
type family JankyFunctorHelper (f :: TYPE r -> TYPE s) (r' :: RuntimeRep) (s' :: RuntimeRep) :: TYPE r' -> TYPE s'

class 
  JankyFunctorHelper f r' s' ~ g,


  JankyFunctor (f :: TYPE r -> TYPE s) (g :: TYPE r' -> TYPE s)

class JankyFunctor (f :: TYPE r -> TYPE s) (g :: TYPE r' -> TYPE s') | f r -> s, g r' -> s', f r' -> g, g r -> f where
  jmap :: forall (a :: TYPE r) (b :: TYPE r'). (a -> b) -> f a -> g b

instance Prelude.Functor f => JankyFunctor (f :: Type -> Type) (f :: Type -> Type) where
  jmap = Prelude.fmap

instance (MaybeRep# r, MaybeRep# r') => JankyFunctor (Maybe# @r) (Maybe# @r') where
  jmap = Maybe.fmap#

instance (MaybeRep r, MaybeRep r') => JankyFunctor (Maybe @r) (Maybe @r') where
  jmap = Maybe.fmap

class JankyControlFunctor (f :: TYPE r -> TYPE s) (g :: TYPE r' -> TYPE s') | f r -> s, g r' -> s', f r' -> g, g r -> f where
  jcmap :: forall (a :: TYPE r) (b :: TYPE r'). (a %1 -> b) %1 -> f a %1 -> g b

instance (STRep r, STRep r') => JankyControlFunctor (ST s @r) (ST s @r') where
  jcmap = fmapST

  
  

class Functor (f :: forall (r :: RuntimeRep). TYPE r -> TYPE s) where
  type FunctorRep f :: RuntimeRep -> Constraint
  fmap :: forall r (a :: TYPE r) rb (b :: TYPE rb). (FunctorRep f r, FunctorRep f rb) => (a -> b) -> f a -> f b

class DataFunctor (f :: forall (r :: RuntimeRep). TYPE r -> TYPE s) where
  type DataFunctorRep f :: RuntimeRep -> Constraint
  dfmap :: forall r (a :: TYPE r) rb (b :: TYPE rb). (DataFunctorRep f r, DataFunctorRep f rb) => (a %1 -> b) -> f a %1 -> f b

class ControlFunctor (f :: forall (r :: RuntimeRep). TYPE r -> TYPE s) where
  type ControlFunctorRep f :: RuntimeRep -> Constraint
  cfmap :: forall r (a :: TYPE r) rb (b :: TYPE rb). (ControlFunctorRep f r, ControlFunctorRep f rb) => (a %1 -> b) %1 -> f a %1 -> f b

class Applicative (m :: forall (r :: RuntimeRep). TYPE r -> TYPE s) where

class Monad (m :: forall (r :: RuntimeRep). TYPE r -> TYPE s) where
  type MonadRep m :: RuntimeRep -> Constraint
  (>>=) :: forall r (a :: TYPE r) rb (b :: TYPE rb). (MonadRep m r, MonadRep m rb) => m a %1 -> (a %1 -> m b) %1 -> m b
  
instance Monad (ST s) where
  type MonadRep (ST s) = STRep
  (>>=) = bindST
-}
