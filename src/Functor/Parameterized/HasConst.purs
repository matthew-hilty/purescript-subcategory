module Control.Subcategory.Functor.Parameterized.HasConst
  ( class HasConst
  , const
  ) where

import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.Functor.HasPure (class HasPure, pure')
import Control.Subcategory.Restrictable (class Restrictable)
import Type.Proxy (Proxy3(Proxy3))

class HasConst
  (c :: Type -> Type -> Type)
  (v :: Type)
  where
  const
    :: forall v'
     . ObjectOf c v
    => ObjectOf c v'
    => v'
    -> c v v'

instance constRestrictableFixedSourceArrow
  :: ( HasPure c (c v)
     , Restrictable Function c
     )
  => HasConst c v
  where
  const x = pure' (Proxy3 :: Proxy3 c) x
