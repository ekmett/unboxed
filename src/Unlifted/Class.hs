{-# Language NoImplicitPrelude #-}
-- | this module is going to be a maintenance nightmare
module Unlifted.Class 
  -- TODO: export everything individually
  ( Eq(..)
  , Ord(..)
  , Num(..)
  , Show(..), ShowList(..), shows
  , Semigroup(..)
  , Monoid(..)
  , print, hPrint
  ) where

import Unlifted.Internal.Class
-- instances
import Unlifted.Rep.Lifted () 
import Unlifted.Rep.Int ()
