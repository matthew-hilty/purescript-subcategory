module Control.Subcategory.Endofunctor.Monad
  ( class Monad
  , ap
  , liftM1
  , whenM
  , unlessM
  ) where

import Prelude (($))

import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.Endofunctor.Applicative (class Applicative)
import Control.Subcategory.Endofunctor.Bind (class Bind)
import Control.Subcategory.Endofunctor.HasBind (class HasBind, bindFlipped)
import Control.Subcategory.Endofunctor.HasPure
  ( class HasPure
  , pure'
  , unless
  , when)
import Control.Subcategory.Endofunctor.HasUnpoint (class HasUnpoint)
import Control.Subcategory.HasUnit (class HasUnit)
import Control.Subcategory.Restrictable (class Restrictable, restrict)
import Control.Subcategory.Slackable (class Slackable, slacken, slacken')
import Type.Proxy (Proxy3(Proxy3))

class (Applicative c m, Bind c m) <= Monad c m

instance monad :: (Applicative c m, Bind c m) => Monad c m

ap
  :: forall c m v0 v1
   . HasBind c m
  => HasPure c m
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c (m v0)
  => ObjectOf c (m v1)
  => ObjectOf c (c v0 v1)
  => ObjectOf c (m (c v0 v1))
  => Restrictable Function c
  => Slackable c
  => m (c v0 v1)
  -> c (m v0) (m v1)
ap mf =
  restrict \mv0 ->
    callOn mf  $ slacken' c $ bindFlipped $ restrict \f ->
    callOn mv0 $ slacken' c $ bindFlipped $ restrict \v0 ->
    slacken (pure' c) $ slacken f v0
  where
  c = Proxy3 :: Proxy3 c

callOn :: forall v0 v1. v0 -> (v0 -> v1) -> v1
callOn x f = f x

liftM1
  :: forall c m v0 v1
   . HasBind c m
  => HasPure c m
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c (m v0)
  => ObjectOf c (m v1)
  => Restrictable Function c
  => Slackable c
  => c v0 v1
  -> c (m v0) (m v1)
liftM1 f =
  restrict \mv0 ->
    callOn mv0 $ slacken' c $ bindFlipped $ restrict \v0 ->
    slacken (pure' c) $ slacken f v0
  where
  c = Proxy3 :: Proxy3 c

whenM
  :: forall c m u
   . HasBind c m
  => HasPure c m
  => HasUnit c u
  => HasUnpoint c u
  => ObjectOf c Boolean
  => ObjectOf c (m Boolean)
  => ObjectOf c (m u)
  => Restrictable Function c
  => Slackable c
  => m Boolean
  -> c (m u) (m u)
whenM mCond =
  restrict \mu ->
    callOn mCond $ slacken' c $ bindFlipped $ restrict \cond ->
    slacken' c (when cond) mu
  where
  c = Proxy3 :: Proxy3 c

unlessM
  :: forall c m u
   . HasBind c m
  => HasPure c m
  => HasUnit c u
  => HasUnpoint c u
  => ObjectOf c Boolean
  => ObjectOf c (m Boolean)
  => ObjectOf c (m u)
  => Restrictable Function c
  => Slackable c
  => m Boolean
  -> c (m u) (m u)
unlessM mCond =
  restrict \mu ->
    callOn mCond $ slacken' c $ bindFlipped $ restrict \cond ->
    slacken' c (unless cond) mu
  where
  c = Proxy3 :: Proxy3 c
