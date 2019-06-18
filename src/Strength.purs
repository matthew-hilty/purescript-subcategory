module Control.Subcategory.Strength
  ( class Strength
  , strengthen
  ) where

import Control.Subcategory.Constituency (class ObjectOf)

class Strength
  (c :: Type -> Type -> Type)
  (t :: Type -> Type -> Type)
  (f :: Type -> Type)
  where
  strengthen
    :: forall v0 v1
     . ObjectOf c v0
    => ObjectOf c v1
    => ObjectOf c (f v1)
    => ObjectOf c (t v0 v1)
    => ObjectOf c (t v0 (f v1))
    => ObjectOf c (f (t v0 v1))
    => t v0 (f v1)
    -> f (t v0 v1)
