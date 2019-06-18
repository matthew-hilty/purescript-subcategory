module Control.Subcategory.ClosedMonoidal
  ( class ClosedMonoidal
  ) where

import Control.Subcategory.ClosedSemimonoidal (class ClosedSemimonoidal)
import Control.Subcategory.HasTUnit (class HasTUnit)

class
  ( ClosedSemimonoidal c t
  , HasTUnit c t u0 u1
  )
    <= ClosedMonoidal c t u0 u1

instance closedMonoidal
  :: ( ClosedSemimonoidal c t
     , HasTUnit c t u0 u1
     )
  => ClosedMonoidal c t u0 u1
