{-# Language DataKinds #-}
{-# Language StandaloneKindSignatures #-}
{-# Language MagicHash #-}
module Internal.IntRep where

import GHC.Types

type Rep :: RuntimeRep
type Rep = 'IntRep
