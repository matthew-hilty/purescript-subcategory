module Control.Subcategory.HasUncurry
  ( class HasUncurry
  , uncurry
  ) where

import Prelude ((<<<))

import Control.Subcategory.Constituency (class ObjectOf)
import Data.Tuple (Tuple)
import Data.Tuple (swap, uncurry) as Tuple

class HasUncurry
  (c      :: Type -> Type -> Type)
  (tensor :: Type -> Type -> Type)
  (exp    :: Type -> Type -> Type)
  where
  uncurry
    :: forall v0 v1 v2
     . ObjectOf c v0
    => ObjectOf c v1
    => ObjectOf c v2
    => ObjectOf c (tensor v0 v1)
    => ObjectOf c (exp v0 v2)
    => c v1 (exp v0 v2)
    -> c (tensor v0 v1) v2

instance hasUncurryFn :: HasUncurry Function Tuple Function where
  uncurry f = Tuple.uncurry f <<< Tuple.swap
