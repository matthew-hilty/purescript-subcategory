module Control.Subcategory.Endofunctor.HasCompose
  ( compose
  , compose'
  ) where

import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.Endofunctor.HasMap (class HasMap, map)
import Control.Subcategory.Restrictable (class Restrictable, restrict)

compose
  :: forall c v0 v1 v2
   . HasMap c (c v0)
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c v2
  => ObjectOf c (c v0 v1)
  => ObjectOf c (c v1 v2)
  => ObjectOf c (c v0 v2)
  => c v1 v2
  -> c (c v0 v1) (c v0 v2)
compose f = map f

compose'
  :: forall c v0 v1 v2
   . HasMap c (c v0)
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c v2
  => ObjectOf c (c v0 v1)
  => ObjectOf c (c v1 v2)
  => ObjectOf c (c v0 v2)
  => ObjectOf c (c (c v0 v1) (c v0 v2))
  => Restrictable Function c
  => c (c v1 v2) (c (c v0 v1) (c v0 v2))
compose' = restrict \f -> compose f
