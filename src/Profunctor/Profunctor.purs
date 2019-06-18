module Control.Subcategory.Profunctor
  ( class Profunctor
  ) where

import Control.Subcategory.Category (class Category)
import Control.Subcategory.Profunctor.HasDimap (class HasDimap)
import Record.Builder (Builder)

class (Category c, HasDimap c p) <= Profunctor c p

instance profunctorBuilder :: HasDimap Builder p => Profunctor Builder p

instance profunctorFn :: HasDimap Function p => Profunctor Function p
