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
import Unlifted.Rep.A ()
import Unlifted.Rep.D ()
import Unlifted.Rep.F ()
import Unlifted.Rep.I ()
import Unlifted.Rep.I8 ()
import Unlifted.Rep.L ()
import Unlifted.Rep.U ()
import Unlifted.Rep.W ()
