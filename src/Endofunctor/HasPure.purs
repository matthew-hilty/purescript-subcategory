module Control.Subcategory.Endofunctor.HasPure
  ( class HasPure
  , pure
  , pure'
  , unless
  , when
  ) where

import Control.Applicative (class Applicative, pure) as Unrestricted
import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.Endofunctor.HasUnpoint (class HasUnpoint, unpoint)
import Control.Subcategory.HasUnit (class HasUnit)
import Control.Subcategory.Restrictable (class Restrictable, restrict)
import Data.Function (const) as Function
import Type.Proxy (Proxy3(Proxy3))

class HasPure c f where
  pure' :: forall v. ObjectOf c v => ObjectOf c (f v) => Proxy3 c -> c v (f v)

instance hasPureUnrestricted
  :: Unrestricted.Applicative f
  => HasPure Function f
  where
  pure' _ = Unrestricted.pure

else instance hasPure
  :: ( ObjectOf c v
     , Restrictable Function c
     )
  => HasPure c (c v)
  where
  pure' _ = restrict \x -> restrict (Function.const x)

pure
  :: forall c f v
   . HasPure c f
  => ObjectOf c v
  => ObjectOf c (f v)
  => c v (f v)
pure = pure' (Proxy3 :: Proxy3 c)

unless
  :: forall c f u
   . HasPure c f
  => HasUnit c u
  => HasUnpoint c u
  => ObjectOf c u
  => ObjectOf c (f u)
  => Restrictable Function c
  => Boolean
  -> c (f u) (f u)
unless true  = restrict \_ -> unpoint (pure :: c u (f u))
unless false = restrict \fu -> fu

when
  :: forall c f u
   . HasPure c f
  => HasUnit c u
  => HasUnpoint c u
  => ObjectOf c u
  => ObjectOf c (f u)
  => Restrictable Function c
  => Boolean
  -> c (f u) (f u)
when true  = restrict \fu -> fu
when false = restrict \_ -> unpoint (pure :: c u (f u))
