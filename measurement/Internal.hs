{-# Language MagicHash #-}
{-# Language RankNTypes #-}
{-# Language DataKinds #-}
{-# Language PolyKinds #-}
{-# OPTIONS_GHC -O2 -fno-prof-auto #-}
-- Make the function applications in nf' and whnf' strict (avoiding allocation)
-- and avoid floating out the computations.
{-# OPTIONS_GHC -fno-full-laziness #-}
module Internal where

import GHC.Types
import Data.Int

whnf'N#
  :: forall (a :: TYPE ('TupleRep '[ 'WordRep, UnliftedRep ])) b.
     (a -> b) -> a -> (Int64 -> IO ())
whnf'N# f x = go where
  go n
    | n <= 0    = pure ()
    | otherwise = f x `seq` go (n-1)
{-# NOINLINE whnf'N# #-}

whnf'B# :: forall (a :: TYPE ('SumRep '[ 'IntRep, LiftedRep ])) b.
  (a -> b) -> a -> (Int64 -> IO ())
whnf'B# f x = go where
  go n
    | n <= 0    = pure ()
    | otherwise = f x `seq` go (n-1)
{-# NOINLINE whnf'B# #-}

whnf'L :: forall a b.
  (a -> b) -> a -> (Int64 -> IO ())
whnf'L f x = go where
  go n
    | n <= 0    = pure ()
    | otherwise = f x `seq` go (n-1)
{-# NOINLINE whnf'L #-}
