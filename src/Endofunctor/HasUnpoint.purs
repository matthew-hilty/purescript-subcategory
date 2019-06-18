module Control.Subcategory.Endofunctor.HasUnpoint
  ( class HasUnpoint
  , unpoint
  ) where

import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.Slackable (slacken)
import Data.Unit (Unit)
import Data.Unit (unit) as Unit
import Record.Builder (Builder)

class
  ObjectOf c u
    <= HasUnpoint
      (c :: Type -> Type -> Type)
      (u :: Type)
      | c -> u
      where
      unpoint
        :: forall v
         . ObjectOf c u
        => ObjectOf c v
        => c u v
        -> v

instance hasUnpointBuilder :: HasUnpoint Builder (Record ()) where
  unpoint builder = slacken builder {}

instance hasUnpointFn :: HasUnpoint Function Unit where
  unpoint f = f Unit.unit
