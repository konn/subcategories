module Control.Subcategory.Productive where
import Data.Constraint (Dict (..))
import Data.Hashable   (Hashable)
import Data.Kind       (Constraint, Type)

class Productive (c :: Type -> Constraint) where
  pair :: (c a, c b) => Dict (c (a, b))

instance Productive Eq where
  pair = Dict

instance Productive Ord where
  pair = Dict

instance Productive Hashable where
  pair = Dict

instance Productive Show where
  pair = Dict

instance Productive Read where
  pair = Dict

instance Productive Bounded where
  pair = Dict

instance Productive Semigroup where
  pair = Dict

instance Productive Monoid where
  pair = Dict
