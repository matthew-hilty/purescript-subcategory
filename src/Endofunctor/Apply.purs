module Control.Subcategory.Endofunctor.Apply
  ( class Apply
  ) where

import Control.Apply (class Apply) as Unrestricted
import Control.Subcategory.Endofunctor.HasApply (class HasApply)
import Control.Subcategory.Endofunctor (class Endofunctor)

class (Endofunctor c f, HasApply c f) <= Apply c f

instance applyUnrestricted :: Unrestricted.Apply f => Apply Function f
