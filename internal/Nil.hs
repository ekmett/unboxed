{-# Language DataKinds #-}
{-# Language StandaloneKindSignatures #-}
module Nil where
import GHC.Types
type Reps :: [RuntimeRep]
type Reps = '[]
