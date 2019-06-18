module Control.Subcategory.Relation.Symmetric
  ( class Symmetric
  , swap
  ) where

import Control.Subcategory.Constituency (class ObjectOf)
import Data.Either (Either(Left, Right))
import Data.Tuple (Tuple)
import Data.Tuple (swap) as Tuple

class Symmetric
  (c :: Type -> Type -> Type)
  (p :: Type -> Type -> Type)
  where
  swap :: forall v0 v1. ObjectOf c v0 => ObjectOf c v1 => p v0 v1 -> p v1 v0

instance symmetricFnEither :: Symmetric Function Either where
  swap = case _ of
    Left x -> Right x
    Right x -> Left x

instance symmetricFnTuple :: Symmetric Function Tuple where
  swap = Tuple.swap
