module Control.Subcategory.HasTJoin
  ( class HasTJoin
  , tJoin
  ) where

import Control.Subcategory.Constituency (class ObjectOf)
import Data.Function (identity) as Function

class HasTJoin
  (c         :: Type -> Type -> Type)
  (bifunctor :: Type -> Type -> Type)
  (tensor    :: Type -> Type -> Type)
  where
  tJoin
    :: forall v0 v1
     . ObjectOf c v0
    => ObjectOf c v1
    => ObjectOf c (tensor v0 v1)
    => bifunctor v0 v1
    -> tensor v0 v1

instance hasTJoinFunction :: HasTJoin Function t t where
  tJoin = Function.identity
