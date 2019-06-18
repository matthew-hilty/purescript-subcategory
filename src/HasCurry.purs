module Control.Subcategory.HasCurry
  ( class HasCurry
  , curry
  ) where

import Prelude ((<<<))

import Control.Subcategory.Constituency (class ObjectOf)
import Data.Function (flip) as Function
import Data.Tuple (Tuple)
import Data.Tuple (curry) as Tuple

class HasCurry
  (c      :: Type -> Type -> Type)
  (tensor :: Type -> Type -> Type)
  (exp    :: Type -> Type -> Type)
  where
  curry
    :: forall v0 v1 v2
     . ObjectOf c v0
    => ObjectOf c v1
    => ObjectOf c v2
    => ObjectOf c (tensor v0 v1)
    => ObjectOf c (exp v0 v2)
    => c (tensor v0 v1) v2
    -> c v1 (exp v0 v2)

instance hasCurryFn :: HasCurry Function Tuple Function where
  curry = Function.flip <<< Tuple.curry
