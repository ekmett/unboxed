{-# Language MagicHash #-}
{-# Language NoImplicitPrelude #-}
module Unlifted.Rep.Tuple
  ( ListDef(Nil, (:#))
  , MaybeDef(Just, Nothing)
  , Maybe#(Just#, Nothing#)
  ) where

import Unlifted.Internal.Maybe
import Unlifted.Rep.Internal.Tuple ()
import TupleDef
