module Control.Subcategory.Restrictable
  ( class Restrictable
  , restrict
  , restrict'
  ) where

import Control.Category (identity) as Unrestricted
import Control.Subcategory.Constituency (class ObjectOf)
import Record.Builder (Builder)
import Type.Proxy (Proxy3(Proxy3))
import Unsafe.Coerce (unsafeCoerce)

class Restrictable
  (c0 :: Type -> Type -> Type)
  (c1 :: Type -> Type -> Type)
  where
  restrict'
    :: forall v0 v1
     . ObjectOf c1 v0
    => ObjectOf c1 v1
    => Proxy3 c1
    -> c0 v0 v1
    -> c1 v0 v1

restrict
  :: forall c0 c1 v0 v1
   . ObjectOf c1 v0
  => ObjectOf c1 v1
  => Restrictable c0 c1
  => c0 v0 v1
  -> c1 v0 v1
restrict = restrict' (Proxy3 :: Proxy3 c1)

instance restrictableFnBuilder :: Restrictable Function Builder where
  restrict' _ = toBuilder
    where
    toBuilder
      :: forall v0 v1
       . ObjectOf Builder v0
      => ObjectOf Builder v1
      => (v0 -> v1)
      -> Builder v0 v1
    toBuilder = unsafeCoerce

instance restrictableFnFn :: Restrictable Function Function where
  restrict' _ = Unrestricted.identity
