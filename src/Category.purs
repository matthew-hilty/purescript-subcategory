module Control.Subcategory.Category
  ( class Category
  ) where

import Control.Subcategory.HasIdentity (class HasIdentity)
import Control.Subcategory.Semigroupoid (class Semigroupoid)

class (HasIdentity c, Semigroupoid c) <= Category (c :: Type -> Type -> Type)

instance category :: (HasIdentity c, Semigroupoid c) => Category c
