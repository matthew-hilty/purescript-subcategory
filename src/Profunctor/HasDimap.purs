module Control.Subcategory.Profunctor.HasDimap
  ( class HasDimap
  , arr
  , dimap
  , lcmap
  , rmap
  , throughUnit
  , unwrapIso
  , wrapIso
  ) where

import Control.Subcategory.Category (class Category)
import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.HasIdentity (class HasIdentity, identity)
import Control.Subcategory.HasTerminate (class HasTerminate, terminate)
import Control.Subcategory.HasCompose ((>>>))
import Control.Subcategory.Relation.Reflexive (class Reflexive, reflect')
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Profunctor (class Profunctor, dimap) as Unrestricted
import Type.Proxy (Proxy3(Proxy3))

class HasDimap
  (c :: Type -> Type -> Type)
  (p :: Type -> Type -> Type)
  where
  dimap
    :: forall v0 v1 v2 v3
     . ObjectOf c v0
    => ObjectOf c v1
    => ObjectOf c v2
    => ObjectOf c v3
    => c v0 v1
    -> c v2 v3
    -> p v1 v2
    -> p v0 v3

arr
  :: forall c p v0 v1
   . HasDimap c p
  => HasIdentity c
  => Reflexive c p
  => ObjectOf c v0
  => ObjectOf c v1
  => c v0 v1
  -> p v0 v1
arr f = rmap f (reflect' (Proxy3 :: Proxy3 c))

lcmap
  :: forall c p v0 v1 v2
   . HasDimap c p
  => HasIdentity c
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c v2
  => c v0 v1
  -> p v1 v2
  -> p v0 v2
lcmap f = dimap f identity

rmap
  :: forall c p v0 v1 v2
   . HasDimap c p
  => HasIdentity c
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c v2
  => c v1 v2
  -> p v0 v1
  -> p v0 v2
rmap f = dimap identity f

throughUnit
  :: forall c u v0 v1
   . HasDimap c c
  => HasIdentity c
  => HasTerminate c u
  => ObjectOf c v0
  => ObjectOf c v1
  => c u v1
  -> c v0 v1
throughUnit f = lcmap (terminate :: c v0 u) f

unwrapIso
  :: forall p v0 v1
   . HasDimap Function p
  => Newtype v0 v1
  => ObjectOf Function v1
  => ObjectOf Function v0
  => p v1 v1
  -> p v0 v0
unwrapIso = dimap unwrap wrap

wrapIso
  :: forall p v0 v1
   . HasDimap Function p
  => Newtype v0 v1
  => ObjectOf Function v1
  => ObjectOf Function v0
  => (v0 -> v1)
  -> p v0 v0
  -> p v1 v1
wrapIso _ = dimap wrap unwrap

instance profunctorUnrestricted
  :: Unrestricted.Profunctor p
  => HasDimap Function p
  where
  dimap = Unrestricted.dimap
else instance profunctorCategory :: Category c => HasDimap c c where
  dimap a2b c2d b2c = a2b >>> b2c >>> c2d
