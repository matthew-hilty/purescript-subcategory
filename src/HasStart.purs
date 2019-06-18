module Control.Subcategory.HasStart
  ( class HasStart
  , start
  , start'
  ) where

import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.HasIntroduce (class HasIntroduce)
import Type.Proxy (Proxy3(Proxy3))

class
  HasIntroduce c s
    <= HasStart
      (c :: Type -> Type -> Type)
      (s :: Type)
      | c -> s
      where
      start' :: ObjectOf c s => Proxy3 c -> s

start :: forall c s. HasStart c s => s
start = start' (Proxy3 :: Proxy3 c)
