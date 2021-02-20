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
-- instances
import Unlifted.Rep.Lifted () 
import Unlifted.Rep.Int ()
import Unlifted.Rep.Word ()
