{-# Language DataKinds #-}
{-# Language StandaloneKindSignatures #-}
module Tuple where
import GHC.Types
import ConsReps
type Rep :: RuntimeRep
type Rep = 'TupleRep Reps
