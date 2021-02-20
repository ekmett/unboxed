-- eventually provide a Prelude alternative that is overloaded to allow unlifted operations where possible

module Unlifted.Prelude
  ( module Unlifted.Class
  , module Unlifted.Syntax
  , module Unlifted.Combinators
  , Bool(..)
  , (&&), (||), not
  , Ordering(..)
  , undefined
  , error
  ) where

import Unlifted.Class
import Unlifted.Syntax
import Unlifted.Combinators
import Prelude
  ( Bool(..)
  , (&&), (||), not
  , Ordering(..)
  , undefined
  , error
  ) 
