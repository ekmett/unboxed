{-# Language DataKinds #-}
{-# Language StandaloneKindSignatures #-}
{-# Language MagicHash #-}
module IntRep where

import GHC.Types

type REP = 'IntRep

type Rep :: RuntimeRep
type Rep = REP
