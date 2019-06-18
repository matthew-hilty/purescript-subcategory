module Control.Subcategory.HasTUnit
  ( class HasTUnit
  , tUnit
  , tUnit'
  ) where

import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.HasUnit (class HasUnit)
import Data.Either (Either)
import Data.Function (identity) as Function
import Data.Tuple (Tuple)
import Data.Unit (Unit)
import Data.Unit (unit) as Unit
import Data.Void (Void)
import Type.Proxy (Proxy3(Proxy3))

class
  HasUnit c u1
    <= HasTUnit
        (c  :: Type -> Type -> Type)
        (bifunctor :: Type -> Type -> Type)
        (u0 :: Type)
        (u1 :: Type)
        | c -> u1
        , bifunctor -> u0
        where
        tUnit' :: ObjectOf c u1 => Proxy3 c -> Proxy3 bifunctor -> u0 -> u1

tUnit
  :: forall c bf u0 u1
   . HasTUnit c bf u0 u1
  => HasUnit c u1
  => u0
  -> u1
tUnit = tUnit' (Proxy3 :: Proxy3 c) (Proxy3 :: Proxy3 bf)

instance hasTUnitFnEither :: HasTUnit Function Either Void Unit where
  tUnit' _ _ _ = Unit.unit

instance hasTUnitFnTuple :: HasTUnit Function Tuple Unit Unit where
  tUnit' _ _ = Function.identity
