{-# Language DataKinds #-}
{-# Language StandaloneKindSignatures #-}
module Sum where
import GHC.Types
import ConsReps
type Rep :: RuntimeRep
type Rep = 'SumRep Reps
