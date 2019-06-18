module Control.Subcategory.Endofunctor
  ( class Endofunctor
  ) where

import Control.Subcategory.Category (class Category)
import Control.Subcategory.Endofunctor.HasMap (class HasMap)
import Record.Builder (Builder)

class (Category c, HasMap c f) <= Endofunctor c f

instance endofunctorFunction :: HasMap Function f => Endofunctor Function f

instance endofunctorBuilder :: HasMap Builder f => Endofunctor Builder f
