module Control.Subcategory.Functor
  ( class Functor
  ) where

import Control.Subcategory.Category (class Category)
import Control.Subcategory.Functor.HasMap (class HasMap)
import Record.Builder (Builder)

class (Category c, HasMap c f) <= Functor c f

instance functorBuilder :: HasMap Builder f => Functor Builder f

instance functorFunction :: HasMap Function f => Functor Function f
