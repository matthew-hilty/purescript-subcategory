module Control.Subcategory.Endofunctor.Discard
  ( class Discard
  , class Discard_
  , discardFlipped
  , discardFlipped_
  ) where

import Control.Bind (class Bind, class Discard, bindFlipped) as Unrestricted
import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.Endofunctor.HasBind (class HasBind, bindFlipped)
import Control.Subcategory.HasUnit (class HasUnit)

class Discard_ c f v where
  discardFlipped_
    :: forall v'
     . HasBind c f
    => ObjectOf c v
    => ObjectOf c (f v)
    => ObjectOf c (f v')
    => c v (f v')
    -> c (f v) (f v')

instance discard_Unrestricted
  :: ( HasBind Function f
     , Unrestricted.Bind f
     , Unrestricted.Discard a
     )
  => Discard_ Function f a
  where
  discardFlipped_ = Unrestricted.bindFlipped

class Discard c v where
  discardFlipped
    :: forall f v'
     . HasBind c f
    => ObjectOf c v
    => ObjectOf c (f v)
    => ObjectOf c (f v')
    => c v (f v')
    -> c (f v) (f v')

instance discardUnit :: HasUnit c u => Discard c u where
  discardFlipped = bindFlipped
