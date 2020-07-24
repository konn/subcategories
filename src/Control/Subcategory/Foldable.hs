module Control.Subcategory.Foldable where
import Control.Subcategory.Functor

class Constrained f => CFoldable f where
  cfoldMap :: (Cat f a, Monoid w) => (a -> w) -> f a -> w
  cfold :: (Cat f w, Monoid w) => f w -> w


