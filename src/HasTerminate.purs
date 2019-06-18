module Control.Subcategory.HasTerminate
  ( class HasTerminate
  , terminate
  , terminate'
  ) where

import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.Restrictable (restrict)
import Data.Unit (Unit)
import Data.Unit (unit) as Unit
import Record.Builder (Builder)
import Type.Proxy (Proxy3(Proxy3))

class
  ObjectOf c u
    <= HasTerminate
      (c :: Type -> Type -> Type)
      (u :: Type)
      | c -> u
      where
      terminate' :: forall v. ObjectOf c u => ObjectOf c v => Proxy3 c -> c v u

terminate
  :: forall c u v
   . HasTerminate c u
  => ObjectOf c v
  => c v u
terminate = terminate' (Proxy3 :: Proxy3 c)

instance hasTerminateBuilder :: HasTerminate Builder (Record ()) where
  terminate' _ = restrict (\_ -> {})

instance hasTerminateFn :: HasTerminate Function Unit where
  terminate' _ _ = Unit.unit
