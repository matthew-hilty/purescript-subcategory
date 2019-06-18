module Control.Subcategory.Functor.Discard
  ( class Discard
  , class Discard_
  , discard
  , discard_
  ) where

import Control.Bind (class Bind, class Discard, bind) as Unrestricted
import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.Functor.HasBind (class HasBind, bind)
import Control.Subcategory.HasUnit (class HasUnit)

class Discard_ c f a where
  discard_
    :: forall v
     . HasBind c f
    => ObjectOf c a
    => ObjectOf c (f v)
    => f a
    -> c a (f v)
    -> f v

instance discard_Unrestricted
  :: ( HasBind Function f
     , Unrestricted.Bind f
     , Unrestricted.Discard a
     )
  => Discard_ Function f a
  where
  discard_ = Unrestricted.bind

class Discard c a where
  discard
    :: forall f v
     . HasBind c f
    => ObjectOf c a
    => ObjectOf c (f v)
    => f a
    -> c a (f v)
    -> f v

instance discardUnit :: HasUnit c u => Discard c u where
  discard = bind
