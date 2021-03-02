{-# Language DataKinds #-}
{-# Language StandaloneKindSignatures #-}
{-# Language ImportQualifiedPost #-}
{-# Language TypeOperators #-}
module ConsReps where
import GHC.Types (RuntimeRep)
import H qualified
import T qualified
type Reps :: [RuntimeRep]
type Reps = H.Rep ': T.Reps
