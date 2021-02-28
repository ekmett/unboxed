{-# Language TypeFamilies #-}
{-# Language DataKinds #-}
{-# Language KindSignatures #-}
module RuntimeRep where

type family Extend (r :: RuntimeRep) (t :: RuntimeRep) :: RuntimeRep where
  Extend a (TupleRep as) = TupleRep (a ': as)
  Extend a (SumRep as) = SumRep (a ': as)
