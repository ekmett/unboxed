{-# Language DataKinds #-}
module UnliftedRep where
import GHC.Types
type Rep = 'BoxedRep 'Unlifted
