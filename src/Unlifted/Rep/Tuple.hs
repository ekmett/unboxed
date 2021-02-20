{-# Language MagicHash #-}
{-# Language NoImplicitPrelude #-}
{-# Language PatternSynonyms #-}
module Unlifted.Rep.Tuple
  ( ListDef(Nil, (:#))
  , MaybeDef(Just, Nothing)
  , Maybe#(Just#, Nothing#)
  ) where

import Unlifted.Internal.Maybe
import Unlifted.Rep.Internal.Tuple ()
import TupleDef
