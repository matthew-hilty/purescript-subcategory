module Control.Subcategory.Monoidal
  ( class Monoidal
  ) where

import Control.Subcategory.HasTUnit (class HasTUnit)
import Control.Subcategory.Semimonoidal (class Semimonoidal)

class (Semimonoidal c bf t, HasTUnit c bf u0 u1) <= Monoidal c bf t u0 u1

instance monoidal
  :: ( Semimonoidal c bf t
     , HasTUnit c bf u0 u1
     )
  => Monoidal c bf t u0 u1
