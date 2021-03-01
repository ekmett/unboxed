{-# Language NoImplicitPrelude #-}
module Unboxed.Class 
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

import Unboxed.Internal.Class
-- official gathering point of instances
import Addr ()
import Double ()
import Float ()
import Int ()
import Int8 ()
import Int16 ()
import Int32 ()
import Int64 ()
import Lifted ()
import Tuple ()
import Sum ()
import Unlifted ()
import Word ()
import Word8 ()
import Word16 ()
import Word32 ()
import Word64 ()
