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
import Unsafe.Coerce (unsafeCoerce)

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
  slacken' _ builder record = coerceBuild Builder.build builder record
    where
    coerceBuild
      :: (forall r1 r2
             . Builder (Record r1) (Record r2)
            -> Record r1
            -> Record r2)
      -> (forall a b
             . ObjectOf Builder a
            => ObjectOf Builder b
            => Builder a b
            -> a
            -> b)
    coerceBuild = unsafeCoerce

instance slackableFn :: Slackable Function where
  slacken' _ = Function.apply
