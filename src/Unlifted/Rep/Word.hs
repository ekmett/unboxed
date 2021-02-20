{-# Language MagicHash #-}
{-# Language NoImplicitPrelude #-}
module Unlifted.Rep.Word
  ( ListDef(Nil, (:#))
  , MaybeDef(Just, Nothing)
  , Maybe#(Just#, Nothing#)
  ) where

import Unlifted.Internal.Maybe
import Unlifted.Rep.Internal.Word ()
import WordDef
