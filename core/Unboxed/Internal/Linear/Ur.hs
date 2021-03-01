{-# Language StandaloneKindSignatures #-}
{-# Language RoleAnnotations #-}
{-# Language MagicHash #-}
{-# Language UnboxedTuples #-}
{-# Language PolyKinds #-}
{-# Language DataKinds #-}
{-# Language GADTs #-}
{-# Language UnliftedNewtypes #-}
{-# Language MultiParamTypeClasses #-}
{-# Language BlockArguments #-}
{-# Language ImportQualifiedPost #-}
{-# Language FlexibleInstances #-}
{-# Language ScopedTypeVariables #-}
{-# Language PatternSynonyms #-}
{-# Language RankNTypes #-}
{-# Language TypeApplications #-}
{-# Language InstanceSigs #-}
{-# Language NoLinearTypes #-}
{-# Language NoImplicitPrelude #-}

module Unboxed.Internal.Linear.Ur where

import GHC.Exts
import Prelude qualified
import Prelude (showParen, showString, showsPrec, ($), (.), (<$>), (>=))

-- an unlifted 'unrestricted' type, implemented as a newtype to avoid a box
-- but generated with a basic arrow rather than a linear arrow using NoLinearTypes
type Ur :: forall (r :: RuntimeRep). TYPE r -> TYPE r
type role Ur representational
newtype Ur a where
  Ur :: forall r (a :: TYPE r). a -> Ur a

{-
instance Show a => Show (Ur (a :: TYPE 'IntRep)) where
  showsPrec d (Ur a) = showParen (d >= 11) $ showString "Ur " . showsPrec 11 a

instance Show a => Show (Ur (a :: TYPE 'UnliftedRep)) where
  showsPrec d (Ur a) = showParen (d >= 11) $ showString "Ur " . showsPrec 11 a
-}

-- this is annoying, because it collides with the kind polymorphic version above
instance Prelude.Show a => Prelude.Show (Ur a) where
  showsPrec d (Ur a) = showParen (d >= 11) $ showString "Ur " . showsPrec 11 a

instance Prelude.Functor Ur where
  fmap f (Ur a) = Ur (f a)
  a <$ _ = Ur a

instance Prelude.Foldable Ur where
  foldMap f (Ur a) = f a

instance Prelude.Traversable Ur where
  traverse f (Ur a) = Ur <$> f a

instance Prelude.Applicative Ur where
  pure = Ur
  Ur f <*> Ur a = Ur (f a)
  _ *> n = n
  m <* _ = m

instance Prelude.Monad Ur where
  Ur a >>= f = f a
  _ >> n = n
