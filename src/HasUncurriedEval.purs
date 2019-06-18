module Control.Subcategory.HasUncurriedEval
  ( class HasUncurriedEval
  , uncurriedEval
  ) where

import Control.Subcategory.Constituency (class ObjectOf)
import Data.Function (apply) as Function
import Data.Tuple (Tuple)
import Data.Tuple (uncurry) as Tuple

class HasUncurriedEval
  (c      :: Type -> Type -> Type)
  (tensor :: Type -> Type -> Type)
  (exp    :: Type -> Type -> Type)
  where
  uncurriedEval
    :: forall v0 v1
     . ObjectOf c (tensor (exp v0 v1) v0)
    => ObjectOf c v1
    => c (tensor (exp v0 v1) v0) v1

instance hasUncurriedEvalFuntion
  :: HasUncurriedEval Function Tuple Function
  where
  uncurriedEval = Tuple.uncurry Function.apply
