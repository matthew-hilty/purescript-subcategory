module Control.Subcategory.Endofunctor.HasBind
  ( class HasBind
  , bind
  , bindFlipped
  , composeKleisli
  , composeKleisliFlipped
  , ifM
  , join
  ) where

import Prelude (flip)

import Control.Bind (class Bind, bindFlipped) as Unrestricted
import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.Endofunctor.HasPure (class HasPure, pure')
import Control.Subcategory.Restrictable (class Restrictable, restrict)
import Control.Subcategory.Slackable (class Slackable, slacken)
import Type.Proxy (Proxy3(Proxy3))

class HasBind c m where
  bindFlipped
    :: forall v0 v1
     . ObjectOf c v0
    => ObjectOf c (m v1)
    => c v0 (m v1)
    -> c (m v0) (m v1)

instance bindUnrestricted
  :: Unrestricted.Bind m
  => HasBind Function m
  where
  bindFlipped = Unrestricted.bindFlipped

bind
  :: forall c m v0 v1
   . HasBind c m
  => ObjectOf c v0
  => ObjectOf c (m v0)
  => ObjectOf c (m v1)
  => ObjectOf c (c v0 (m v1))
  => Restrictable Function c
  => Slackable c
  => m v0
  -> c (c v0 (m v1)) (m v1)
bind mx0 =
  restrict \f ->
    slacken (bindFlipped f) mx0

composeKleisli
  :: forall c m v0 v1 v2
   . HasBind c m
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c (m v1)
  => ObjectOf c (m v2)
  => ObjectOf c (c v0 (m v1))
  => Restrictable Function c
  => Slackable c
  => c v0 (m v1)
  -> c v1 (m v2)
  -> c v0 (m v2)
composeKleisli f0 f1 =
  restrict \x0 ->
    slacken (bindFlipped f1) (slacken f0 x0)

composeKleisliFlipped
  :: forall c m v0 v1 v2
   . HasBind c m
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c (m v1)
  => ObjectOf c (m v2)
  => ObjectOf c (c v0 (m v1))
  => Restrictable Function c
  => Slackable c
  => c v1 (m v2)
  -> c v0 (m v1)
  -> c v0 (m v2)
composeKleisliFlipped = flip composeKleisli

ifM
  :: forall c m v
   . HasBind c m
  => ObjectOf c Boolean
  => ObjectOf c (m Boolean)
  => ObjectOf c (m v)
  => ObjectOf c (c (m v) (m v))
  => Restrictable Function c
  => Slackable c
  => m Boolean
  -> c (m v) (c (m v) (m v))
ifM mCond =
    restrict \mt ->
      restrict \mf ->
        let
          predicate :: c Boolean (m v)
          predicate = restrict (if _ then mt else mf)
        in slacken (bindFlipped predicate) mCond

join
  :: forall c m v
   . HasBind c m
  => HasPure c m
  => ObjectOf c v
  => ObjectOf c (m v)
  => ObjectOf c (m (m v))
  => c (m (m v)) (m v)
join =
  bindFlipped (bindFlipped (pure' (Proxy3 :: Proxy3 c)))
