-- eventually provide a Prelude alternative that is overloaded to allow unlifted operations where possible

module Unlifted.Prelude
  ( 
  -- * Classes
    module Unlifted.Class
  , module Unlifted.List
  , module Unlifted.Maybe
  -- * Automatically levitating if then else syntax
  , ifThenElse
  -- * Printing unlifted values
  , PrintRep(hPrint)
  -- * Unlifted.Combinators
  , id
  , const
  -- * Standard Prelude types
  , Bool(..), (&&), (||), not, otherwise
  , Ordering(..)
  , undefined
  , error
  , ($) -- has a magic rule we can't beat
  ) where

import Unlifted.Class
import Unlifted.Combinators
import Unlifted.List
import Unlifted.Maybe
import Unlifted.Rep
import Unlifted.Syntax
import Prelude
  ( Bool(..)
  , (&&), (||), not, otherwise
  , Ordering(..)
  , undefined
  , error
  , ($)
  ) 

