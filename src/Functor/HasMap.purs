module Control.Subcategory.Functor.HasMap
  ( class HasMap
  , map        , (<$>)
  , mapFlipped , (<#>)
  ) where

import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.Restrictable (class Restrictable, restrict)
import Control.Subcategory.Slackable (class Slackable, slacken)
import Data.Functor (class Functor, map) as Unrestricted

class HasMap
  (c :: Type -> Type -> Type)
  (f :: Type -> Type)
  where
  map
    :: forall v0 v1
     . ObjectOf c v0
    => ObjectOf c v1
    => c v0 v1
    -> f v0
    -> f v1

infixl 4 map as <$>

instance hasMapUnrestricted
  :: Unrestricted.Functor f
  => HasMap Function f
  where
  map = Unrestricted.map

flap
  :: forall c f v0 v1
   . HasMap c f
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c (c v0 v1)
  => Restrictable Function c
  => Slackable c
  => f (c v0 v1)
  -> v0
  -> f v1
flap ff x = map consumeX ff
  where
  consumeX :: c (c v0 v1) v1
  consumeX = restrict (\f -> slacken f x)

infixl 4 flap as <@>

mapFlipped
  :: forall c f v0 v1
   . HasMap c f
  => ObjectOf c v0
  => ObjectOf c v1
  => f v0
  -> c v0 v1
  -> f v1
mapFlipped fa f = f <$> fa

infixl 1 mapFlipped as <#>
