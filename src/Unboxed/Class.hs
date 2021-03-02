{-# Language NoImplicitPrelude #-}
module Unboxed.Class 
  -- TODO: export everything individually
  ( Eq(..), EqRep
  , Ord(..), OrdRep
  , Bounded(..)
  , Num(..), NumRep
  , Fractional(..), FractionalRep
  , Real(..), RealRep
  , Enum(..), EnumRep
  , Integral(..), IntegralRep
  , RealFrac(..), RealFracRep
  , Floating(..), FloatingRep
  , RealFloat(..), RealFloatRep
  , Show(..), ShowRep
  , ShowList(..), ShowListRep
  , shows
  , Semigroup(..)
  , Monoid(..)
  , Functor(..)
  , PrintRep
  , print, hPrint
  ) where

import Unboxed.Internal.Class
-- official gathering point of instances
import Unboxed.Rep.Addr ()
import Unboxed.Rep.Double ()
import Unboxed.Rep.Float ()
import Unboxed.Rep.Int ()
import Unboxed.Rep.Int8 ()
import Unboxed.Rep.Int16 ()
import Unboxed.Rep.Int32 ()
import Unboxed.Rep.Int64 ()
import Unboxed.Rep.Lifted ()
import Unboxed.Rep.Tuple ()
import Unboxed.Rep.Sum ()
import Unboxed.Rep.Unlifted ()
import Unboxed.Rep.Word ()
import Unboxed.Rep.Word8 ()
import Unboxed.Rep.Word16 ()
import Unboxed.Rep.Word32 ()
import Unboxed.Rep.Word64 ()
