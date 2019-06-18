module Control.Subcategory.Closed
  ( class Closed
  , assertClosed
  ) where

import Control.Subcategory.Constituency (class ObjectOf)
import Data.Unit (Unit)
import Data.Unit (unit) as Unit
import Record.Builder (Builder)

class Closed (c :: Type -> Type -> Type) where
  assertClosed :: forall v0 v1. ObjectOf c (c v0 v1) => Unit

instance closedFn :: Closed Function where
  assertClosed = Unit.unit

instance closedBuilder :: Closed Builder where
  assertClosed = Unit.unit
