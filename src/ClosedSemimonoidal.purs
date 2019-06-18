module Control.Subcategory.ClosedSemimonoidal
  ( class ClosedSemimonoidal
  ) where

import Control.Subcategory.Closed (class Closed)
import Control.Subcategory.HasCurry (class HasCurry)
import Control.Subcategory.HasUncurry (class HasUncurry)

class
  ( Closed c
  , HasCurry c tensor c
  , HasUncurry c tensor c
  )
  <= ClosedSemimonoidal c tensor

instance closedSemimonoidal
  :: ( Closed c
     , HasCurry c tensor c
     , HasUncurry c tensor c
     )
  => ClosedSemimonoidal c tensor
