module Control.Subcategory.Endofunctor.HasMap
  ( class HasMap
  , flap
  , map
  ) where

import Control.Subcategory.HasCompose (class HasCompose, (>>>))
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
    => ObjectOf c (f v0)
    => ObjectOf c (f v1)
    => c v0 v1
    -> c (f v0) (f v1)

instance hasMapUnrestricted
  :: Unrestricted.Functor f
  => HasMap Function f
  where
  map = Unrestricted.map
else instance hasMapFixedSourceArrow
  :: ( ObjectOf c v
     , ObjectOf c v'
     , ObjectOf c (c v v')
     , Restrictable Function c
     , HasCompose c
     )
  => HasMap c (c v)
  where
  map f' = restrict \f -> f >>> f'

flap
  :: forall c f v0 v1
   . HasMap c f
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c (c v0 v1)
  => ObjectOf c (c (c v0 v1) v1)
  => ObjectOf c (f v1)
  => ObjectOf c (f (c v0 v1))
  => ObjectOf c (c (f (c v0 v1)) (f v1))
  => ObjectOf c (c v0 (c (c v0 v1) v1))
  => Restrictable Function c
  => Slackable c
  => f (c v0 v1)
  -> c v0 (f v1)
flap ff = restrict \x -> slacken (map (slacken consume x)) ff
  where
  consume :: c v0 (c (c v0 v1) v1)
  consume = restrict \x -> restrict \f -> slacken f x
