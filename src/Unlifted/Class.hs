{-# Language NoImplicitPrelude #-}
module Unlifted.Class 
  -- TODO: export everything individually
  ( Eq(..)
  , Ord(..)
  , Bounded(..)
  , Num(..)
  , Fractional(..)
  , Real(..)
  , Enum(..)
  , Integral(..)
  , RealFrac(..)
  , Floating(..)
  , RealFloat(..)
  , Show(..), ShowList(..), shows
  , Semigroup(..)
  , Monoid(..)
  , Functor(..)
  , print, hPrint
  ) where

import Unlifted.Internal.Class
-- official gathering point of instances
import A ()
import D ()
import F ()
import I ()
import I8 ()
import I16 ()
import I32 ()
import I64 ()
import L ()
import T0 ()
import S0 ()
import U ()
import W ()
import W8 ()
import W16 ()
import W32 ()
import W64 ()
