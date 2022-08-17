{-# Language DataKinds #-}
module LiftedRep where
import GHC.Types
type Rep = 'BoxedRep 'Lifted
