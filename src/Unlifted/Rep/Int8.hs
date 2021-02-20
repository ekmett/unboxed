{-# Language MagicHash #-}
{-# Language NoImplicitPrelude #-}
{-# Language PatternSynonyms #-}
module Unlifted.Rep.Int8
  ( ListDef(Nil, (:#))
  , MaybeDef(Just, Nothing)
  , Maybe#(Just#, Nothing#)
  ) where

import Unlifted.Internal.Maybe
import Unlifted.Rep.Internal.Int8 ()
import Def.Int8
