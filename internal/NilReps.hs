{-# Language DataKinds #-}
{-# Language StandaloneKindSignatures #-}
module NilReps where
import GHC.Types
type Reps :: [RuntimeRep]
type Reps = '[]
