module Control.Subcategory.Slackable
  ( class Slackable
  , slacken
  , slacken'
  ) where

import Control.Subcategory.Constituency (class ObjectOf)
import Data.Function (apply) as Function
import Record.Builder (Builder)
import Record.Builder (build) as Builder
import Type.Proxy (Proxy3(Proxy3))

class Slackable (c :: Type -> Type -> Type) where
  slacken'
    :: forall v0 v1
     . ObjectOf c v0
    => ObjectOf c v1
    => Proxy3 c
    -> c v0 v1
    -> v0
    -> v1

slacken
  :: forall c v0 v1
   . ObjectOf c v0
  => ObjectOf c v1
  => Slackable c
  => c v0 v1
  -> v0
  -> v1
slacken = slacken' (Proxy3 :: Proxy3 c)

instance slackableBuilder :: Slackable Builder where
  slacken' _ builder record = slackenBuilder Builder.build builder record

foreign import slackenBuilder
  :: forall v0 v1
   . ObjectOf Builder v0
  => ObjectOf Builder v1
  => (forall r0 r1. Builder (Record r0) (Record r1) -> Record r0 -> Record r1)
  -> Builder v0 v1
  -> v0
  -> v1

instance slackableFn :: Slackable Function where
  slacken' _ = Function.apply
