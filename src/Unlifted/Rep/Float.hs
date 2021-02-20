{-# Language MagicHash #-}
{-# Language NoImplicitPrelude #-}
module Unlifted.Rep.Float
  ( ListDef(Nil, (:#))
  , MaybeDef(Just, Nothing)
  , Maybe#(Just#, Nothing#)
  ) where

import Unlifted.Internal.Maybe
import Unlifted.Rep.Internal.Float ()
import FloatDef
