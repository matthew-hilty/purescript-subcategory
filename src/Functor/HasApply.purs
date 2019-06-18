module Control.Subcategory.Functor.HasApply
  ( class HasApply
  , apply       , (<*>)
  , applyFirst  , (<*)
  , applySecond , (*>)
  , lift2
  , lift3
  , lift4
  , lift5
  ) where

import Control.Apply (class Apply, apply) as Unrestricted
import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.Endofunctor.HasConst
  ( class HasConst
  , const
  ) as Endofunctor
import Control.Subcategory.Functor.HasConst (class HasConst, const) as Functor
import Control.Subcategory.Functor.HasMap (class HasMap, map, (<$>))
import Control.Subcategory.HasIdentity (class HasIdentity, identity)

class HasApply c f where
  apply
    :: forall v0 v1
     . ObjectOf c v0
    => ObjectOf c v1
    => ObjectOf c (c v0 v1)
    => f (c v0 v1)
    -> f v0
    -> f v1

infixl 4 apply as <*>

instance applyUnrestricted :: Unrestricted.Apply f => HasApply Function f where
  apply = Unrestricted.apply

applyFirst
  :: forall c f v0 v1
   . HasApply c f
  => Endofunctor.HasConst c
  => HasMap c f
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c (c v1 v0)
  => f v0
  -> f v1
  -> f v0
applyFirst x0 x1 = apply constX0 x1
  where
  constX0 :: f (c v1 v0)
  constX0 = Endofunctor.const <$> x0

infixl 4 applyFirst as <*

applySecond
  :: forall c f v0 v1
   . HasApply c f
  => Functor.HasConst c
  => HasIdentity c
  => HasMap c f
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c (c v1 v1)
  => ObjectOf c (c v0 (c v1 v1))
  => ObjectOf c (c (c v1 v1) (c v0 (c v1 v1)))
  => f v0
  -> f v1
  -> f v1
applySecond x0 x1 =
    apply (map evalConstIdentity x0) x1
  where
  evalConstIdentity :: c v0 (c v1 v1)
  evalConstIdentity = Functor.const identity

infixl 4 applySecond as *>

lift2
  :: forall c f v0 v1 v2
   . HasApply c f
  => HasMap c f
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c v2
  => ObjectOf c (c v1 v2)
  => c v0 (c v1 v2)
  -> f v0
  -> f v1
  -> f v2
lift2 f x0 x1 = f <$> x0 <*> x1

lift3
  :: forall c f v0 v1 v2 v3
   . HasApply c f
  => HasMap c f
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c v2
  => ObjectOf c v3
  => ObjectOf c (c v2 v3)
  => ObjectOf c (c v1 (c v2 v3))
  => c v0 (c v1 (c v2 v3))
  -> f v0
  -> f v1
  -> f v2
  -> f v3
lift3 f x0 x1 x2 = f <$> x0 <*> x1 <*> x2

lift4
  :: forall c f v0 v1 v2 v3 v4
   . HasApply c f
  => HasMap c f
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c v2
  => ObjectOf c v3
  => ObjectOf c v4
  => ObjectOf c (c v3 v4)
  => ObjectOf c (c v2 (c v3 v4))
  => ObjectOf c (c v1 (c v2 (c v3 v4)))
  => c v0 (c v1 (c v2 (c v3 v4)))
  -> f v0
  -> f v1
  -> f v2
  -> f v3
  -> f v4
lift4 f x0 x1 x2 x3 = f <$> x0 <*> x1 <*> x2 <*> x3

lift5
  :: forall c f v0 v1 v2 v3 v4 v5
   . HasApply c f
  => HasMap c f
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c v2
  => ObjectOf c v3
  => ObjectOf c v4
  => ObjectOf c v5
  => ObjectOf c (c v4 v5)
  => ObjectOf c (c v3 (c v4 v5))
  => ObjectOf c (c v2 (c v3 (c v4 v5)))
  => ObjectOf c (c v1 (c v2 (c v3 (c v4 v5))))
  => c v0 (c v1 (c v2 (c v3 (c v4 v5))))
  -> f v0
  -> f v1
  -> f v2
  -> f v3
  -> f v4
  -> f v5
lift5 f x0 x1 x2 x3 x4 = f <$> x0 <*> x1 <*> x2 <*> x3 <*> x4
