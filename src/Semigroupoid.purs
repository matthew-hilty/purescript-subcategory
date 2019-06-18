module Control.Subcategory.Semigroupoid
  ( class Semigroupoid
  ) where

import Control.Subcategory.HasCompose (class HasCompose)

class HasCompose s <= Semigroupoid (s :: Type -> Type -> Type)

instance semigroupoidHasCompose :: HasCompose s => Semigroupoid s
