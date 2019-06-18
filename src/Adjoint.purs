module Control.Subcategory.Adjoint
  ( class Adjoint
  , class HasToLeft
  , class HasToRight
  , toLeft
  , toRight
  )
  where

import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.HasCurry (class HasCurry, curry)
import Control.Subcategory.HasUncurry (class HasUncurry, uncurry)

class (HasToLeft c0 c1 l r, HasToRight c0 c1 l r) <= Adjoint c0 c1 l r

instance adjoint
  :: ( HasToLeft c0 c1 l r
     , HasToRight c0 c1 l r
     )
  => Adjoint c0 c1 l r

class HasToLeft
  (c0 :: Type -> Type -> Type)
  (c1 :: Type -> Type -> Type)
  (l  :: Type -> Type)
  (r  :: Type -> Type)
  where
  toLeft
    :: forall v0 v1
     . ObjectOf c0 (l v0)
    => ObjectOf c0 v1
    => ObjectOf c1 v0
    => ObjectOf c1 (r v1)
    => c1 v0 (r v1)
    -> c0 (l v0) v1

instance hasToLeftHasUncurry
  :: ( HasUncurry c tensor exp
     , ObjectOf c v
     )
  => HasToLeft c c (tensor v) (exp v)
  where
  toLeft = uncurry

class HasToRight
  (c0 :: Type -> Type -> Type)
  (c1 :: Type -> Type -> Type)
  (l  :: Type -> Type)
  (r  :: Type -> Type)
  where
  toRight
    :: forall v0 v1
     . ObjectOf c0 (l v0)
    => ObjectOf c0 v1
    => ObjectOf c1 v0
    => ObjectOf c1 (r v1)
    => c0 (l v0) v1
    -> c1 v0 (r v1)

instance hasToRightHasCurry
  :: ( HasCurry c tensor exp
     , ObjectOf c v
     )
  => HasToRight c c (tensor v) (exp v)
  where
  toRight = curry
