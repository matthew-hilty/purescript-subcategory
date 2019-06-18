module Control.Subcategory.Endofunctor.HasPoint
  ( class HasPoint
  , const
  , point
  ) where

import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.HasIdentity (class HasIdentity)
import Control.Subcategory.HasUnit (class HasUnit)
import Control.Subcategory.Profunctor.HasDimap (class HasDimap, throughUnit)
import Control.Subcategory.Restrictable (restrict)
import Data.Unit (Unit)
import Record.Builder (Builder)

class
  ObjectOf c u
    <= HasPoint
      (c :: Type -> Type -> Type)
      (u :: Type)
      | c -> u  -- This fundep is to accommodate the category `Builder`.
      where
      point :: forall v. ObjectOf c v => v -> c u v

instance hasPointFn :: HasPoint Function Unit where
  point v _ = v

instance hasPointBuilder :: HasPoint Builder (Record ()) where
  point record = restrict (\_ -> record)

const
  :: forall c u v0 v1
   . HasDimap c c
  => HasIdentity c
  => HasPoint c u
  => HasUnit c u
  => ObjectOf c v0
  => ObjectOf c v1
  => v1
  -> c v0 v1
const = point1
  where
  point0 :: v1 -> c u v1
  point0 v = point v
  point1 :: v1 -> c v0 v1
  point1 v = throughUnit (point0 v)
