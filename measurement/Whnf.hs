{-# Language MagicHash #-}
{-# Language RankNTypes #-}
{-# Language DataKinds #-}
{-# Language PolyKinds #-}
module Whnf where

import Criterion.Measurement.Types
import GHC.Types
import Data.Int

whnf'N#
  :: forall (a :: TYPE ('TupleRep '[ 'WordRep, 'UnliftedRep ])) b.
     (a -> b) -> a -> (Int64 -> IO ())
whnf'N# f x = go where
  go n
    | n <= 0    = pure ()
    | otherwise = f x `seq` go (n-1)
{-# NOINLINE whnf'N# #-}

whnfN#
  :: forall (a :: TYPE ('TupleRep '[ 'WordRep, 'UnliftedRep ])) b.
     (a -> b) -> a -> Benchmarkable
whnfN# f x = toBenchmarkable (whnf'N# f x)

whnf'B# :: forall (a :: TYPE ('SumRep '[ 'IntRep, 'LiftedRep ])) b.
  (a -> b) -> a -> (Int64 -> IO ())
whnf'B# f x = go where
  go n
    | n <= 0    = pure ()
    | otherwise = f x `seq` go (n-1)
{-# NOINLINE whnf'B# #-}

whnfB#
  :: forall (a :: TYPE ('SumRep '[ 'IntRep, 'LiftedRep ])) b.
     (a -> b) -> a -> Benchmarkable
whnfB# f x = toBenchmarkable (whnf'B# f x)

whnf'L :: forall a b.
  (a -> b) -> a -> (Int64 -> IO ())
whnf'L f x = go where
  go n
    | n <= 0    = pure ()
    | otherwise = f x `seq` go (n-1)
{-# NOINLINE whnf'L #-}

whnfL
  :: forall a b.
     (a -> b) -> a -> Benchmarkable
whnfL f x = toBenchmarkable (whnf'L f x)
