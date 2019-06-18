module Control.Subcategory.Relation.Reflexive
  ( class Reflexive
  , reflect
  , reflect'
  ) where

import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.HasIdentity (class HasIdentity, identity)
import Type.Proxy (Proxy3(Proxy3))

class Reflexive
  (c :: Type -> Type -> Type)
  (p :: Type -> Type -> Type)
  where
  reflect' :: forall v. ObjectOf c v => Proxy3 c -> p v v

reflect
  :: forall c p v
   . Reflexive c p
  => ObjectOf c v
  => p v v
reflect = reflect' (Proxy3 :: Proxy3 c)

instance reflexiveHasIdentity :: HasIdentity c => Reflexive c c where
  reflect' _ = identity
