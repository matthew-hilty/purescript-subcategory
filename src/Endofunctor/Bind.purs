module Control.Subcategory.Endofunctor.Bind
  ( class Bind
  ) where

import Control.Bind (class Bind) as Unrestricted
import Control.Subcategory.Endofunctor.Apply (class Apply)
import Control.Subcategory.Endofunctor.HasBind (class HasBind)

class (Apply c f, HasBind c f) <= Bind c f

instance bindUnrestricted :: Unrestricted.Bind m => Bind Function m
