module Control.Subcategory.HasIntroduce
  ( class HasIntroduce
  , introduce
  , introduce'
  ) where

import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.Restrictable (restrict)
import Data.Void (Void)
import Data.Void (absurd) as Void
import Record.Builder (Builder)
import Type.Proxy (Proxy3(Proxy3))

class
  ObjectOf c s
    <= HasIntroduce
      (c :: Type -> Type -> Type)
      (s :: Type)
      | c -> s
      where
      introduce' :: forall v. ObjectOf c s => ObjectOf c v => Proxy3 c -> c s v

introduce
  :: forall c s v
   . HasIntroduce c s
  => ObjectOf c v
  => c s v
introduce = introduce' (Proxy3 :: Proxy3 c)

instance hasIntroduceBuilder :: HasIntroduce Builder Void where
  introduce' _ = restrict Void.absurd

instance hasIntroduceFn :: HasIntroduce Function Void where
  introduce' _ = Void.absurd
