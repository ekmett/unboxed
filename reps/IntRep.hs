{-# Language DataKinds #-}
{-# Language StandaloneKindSignatures #-}
module IntRep where
import GHC.Types

type Rep :: RuntimeRep
type Rep = 'IntRep
