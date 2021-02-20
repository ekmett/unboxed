{-# Language MagicHash #-}
{-# Language NoImplicitPrelude #-}
{-# Language PatternSynonyms #-}
module Unlifted.Rep.Double
  ( ListDef(Nil, (:#))
  , MaybeDef(Just, Nothing)
  , Maybe#(Just#, Nothing#)
  ) where

import Unlifted.Internal.Maybe
import Unlifted.Rep.Internal.Double ()
import Def.Double
