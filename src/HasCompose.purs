module Control.Subcategory.HasCompose
  ( class HasCompose
  , compose        , (<<<)
  , composeFlipped , (>>>)
  ) where

import Control.Semigroupoid (compose) as Unrestricted
import Control.Subcategory.Constituency (class ObjectOf)
import Record.Builder (Builder)

class HasCompose (s :: Type -> Type -> Type) where
  compose
    :: forall v0 v1 v2
     . ObjectOf s v0
    => ObjectOf s v1
    => ObjectOf s v2
    => s v1 v2
    -> s v0 v1
    -> s v0 v2

infixr 9 compose as <<<

composeFlipped
  :: forall s v0 v1 v2
   . ObjectOf s v0
  => ObjectOf s v1
  => ObjectOf s v2
  => HasCompose s
  => s v0 v1
  -> s v1 v2
  -> s v0 v2
composeFlipped f g = compose g f

infixr 9 composeFlipped as >>>

instance hasComposeBuilder :: HasCompose Builder where
  compose = Unrestricted.compose

instance hasComposeFn :: HasCompose Function where
  compose = Unrestricted.compose
