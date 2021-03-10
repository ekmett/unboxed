{-# Language MagicHash #-}
-- eventually provide a Prelude alternative that is overloaded to allow unlifted operations where possible

module Unboxed.Prelude
  ( 
  -- * Classes
    module Unboxed.Class
  , module Unboxed.Levitation
  , module Unboxed.List
  , module Unboxed.Maybe
  -- * Automatically levitating if then else syntax
  , ifThenElse
  -- * Printing unlifted values
  , PrintRep(hPrint)
  -- * Unboxed.Combinators
  , id
  , const
  -- * Standard Prelude types
  , Bool(..), (&&), (||), not, otherwise
  , Ordering(..)
  , undefined
  , error
  , ($) -- has a magic rule we can't beat

  -- unlifted numerics
  , Natural#
  , Int#, Int8#, Int16#, Int32#, Int64#
  , Word#, Word8#, Word16#, Word32#, Word64#
  , Float#, Double#
  ) where

import GHC.Prim
import Unboxed.Class
import Unboxed.Combinators
import Unboxed.Levitation
import Unboxed.List
import Unboxed.Maybe
import Unboxed.Natural
import Unboxed.Syntax
import Prelude
  ( Bool(..)
  , (&&), (||), not, otherwise
  , Ordering(..)
  , undefined
  , error
  , ($)
  ) 
