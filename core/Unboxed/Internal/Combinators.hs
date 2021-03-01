{-# Language DataKinds #-}
{-# Language NoImplicitPrelude #-}
{-# Language PolyKinds #-}
{-# Language TypeFamilies #-}
{-# Language RankNTypes #-}

module Unboxed.Internal.Combinators where

import GHC.Types (TYPE)
import Unboxed.Levitation

-- i don't want to levitate the argument. make a class?
id :: forall r (a :: TYPE r). Lev a -> a
id a = a
{-# INLINE id #-}

-- i should levitate b, but i shouldn't levitate a. put in same class?
const :: forall r (a :: TYPE r) r' (b :: TYPE r'). Lev a -> Lev b -> a
const a _ = a
{-# INLINE const #-}
