module Control.Subcategory.Endofunctor.Applicative
  ( class Applicative
  , liftA1
  ) where

import Control.Applicative (class Applicative) as Unrestricted
import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.Endofunctor.Apply (class Apply)
import Control.Subcategory.Endofunctor.HasApply (class HasApply, apply)
import Control.Subcategory.Endofunctor.HasPure (class HasPure, pure')
import Control.Subcategory.Restrictable (class Restrictable, restrict)
import Control.Subcategory.Slackable (class Slackable, slacken)
import Type.Proxy (Proxy3(Proxy3))

class (Apply c f, HasPure c f) <= Applicative c f

instance applicativeUnrestricted
  :: Unrestricted.Applicative f
  => Applicative Function f

liftA1
  :: forall c f v0 v1
   . HasApply c f
  => HasPure c f
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c (f v0)
  => ObjectOf c (f v1)
  => ObjectOf c (c v0 v1)
  => ObjectOf c (f (c v0 v1))
  => ObjectOf c (c (f v0) (f v1))
  => Restrictable Function c
  => Slackable c
  => c v0 v1
  -> c (f v0) (f v1)
liftA1 f =
  restrict \fv0 ->
    (slacken (apply (slacken (pure' (Proxy3 :: Proxy3 c)) f))) fv0
