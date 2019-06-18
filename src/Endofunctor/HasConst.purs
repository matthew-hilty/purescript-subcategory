module Control.Subcategory.Endofunctor.HasConst
  ( class HasConst
  , const
  ) where

import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.Functor.HasPure (class HasPure, pure')
import Control.Subcategory.Restrictable (class Restrictable, restrict)
import Type.Proxy (Proxy3(Proxy3))

class HasConst (c :: Type -> Type -> Type) where
  const
    :: forall v0 v1
     . ObjectOf c v0
    => ObjectOf c v1
    => ObjectOf c (c v1 v0)
    => c v0 (c v1 v0)

instance constRestrictableFixedSourceArrow
  :: ( HasPure c (c v)
     , Restrictable Function c
     )
  => HasConst c
  where
  const = restrict \v -> pure' (Proxy3 :: Proxy3 c) v
