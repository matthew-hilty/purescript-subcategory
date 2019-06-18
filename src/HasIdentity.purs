module Control.Subcategory.HasIdentity
  ( class HasIdentity
  , identity
  ) where

import Control.Category (identity) as Unrestricted
import Control.Subcategory.Constituency (class ObjectOf)
import Record.Builder (Builder)

class HasIdentity (c :: Type -> Type -> Type) where
  identity :: forall a. ObjectOf c a => c a a

instance hasIdentityBuilder :: HasIdentity Builder where
  identity = Unrestricted.identity

instance hasIdentityFn :: HasIdentity Function where
  identity = Unrestricted.identity
