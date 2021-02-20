-- | this module is going to be a maintenance nightmare
module Unlifted.Class 
  -- TODO: export everything individually
  ( module Unlifted.Internal.Class
  ) where

import Unlifted.Internal.Class
-- instances
import Unlifted.Rep.Lifted () 
import Unlifted.Rep.Int ()
