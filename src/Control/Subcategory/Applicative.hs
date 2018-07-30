{-# LANGUAGE EmptyCase, TupleSections, UndecidableSuperClasses #-}
module Control.Subcategory.Applicative
  ( CApplicative(..), defaultRightApply, defaultLeftApply
  ) where
import Control.Subcategory.Functor

import qualified Control.Applicative             as App
import qualified Control.Monad.ST.Lazy           as LST
import qualified Control.Monad.ST.Strict         as SST
import           Data.Coerce                     (coerce)
import           Data.Functor.Const              (Const)
import           Data.Functor.Identity           (Identity)
import qualified Data.Functor.Product            as SOP
import           Data.Hashable                   (Hashable)
import qualified Data.HashMap.Strict             as HM
import qualified Data.HashSet                    as HS
import qualified Data.IntMap                     as IM
import           Data.List.NonEmpty              (NonEmpty)
import qualified Data.Map                        as Map
import qualified Data.Semigroup                  as Sem
import qualified Data.Sequence                   as Seq
import qualified Data.Set                        as Set
import qualified Data.Tree                       as Tree
import           GHC.Conc                        (STM)
import           Text.ParserCombinators.ReadP    (ReadP)
import           Text.ParserCombinators.ReadPrec (ReadPrec)

infixl 4 <.>
class CFunctor f => CApplicative f where
  pair :: (Cat f a, Cat f b) => f a -> f b -> f (a, b)
  default pair :: (Applicative f) => f a -> f b -> f (a, b)
  pair = App.liftA2 (,)
  (<.>) :: (Cat f a, Cat f b, Cat f (a -> b)) => f (a -> b) -> f a -> f b
  default (<.>) :: (Applicative f) => f (a -> b) -> f a -> f b
  (<.>) = (<*>)
  (.>) :: (Cat f a, Cat f b) => f a -> f b -> f b
  default (.>) :: Applicative f
               => f a -> f b -> f b
  (.>) = (*>)
  (<.) :: (Cat f a, Cat f b) => f a -> f b -> f a
  default (<.) :: Applicative f
               => f a -> f b -> f a
  (<.) = (<*)

defaultLeftApply :: (Cat f (b1, b2), Cat f b1, Cat f b2, CApplicative f)
                 => f b1 -> f b2 -> f b1
defaultLeftApply a b = uncurry const <$:> pair a b
defaultRightApply :: (Cat f (b1, b2), Cat f b2, Cat f b1, CApplicative f)
                  => f b1 -> f b2 -> f b2
defaultRightApply a b = uncurry (const id) <$:> pair a b

instance Semigroup w => CApplicative (Const w) where
  pair = coerce @(w -> w -> w) (<>)
  (<.>) = coerce @(w -> w -> w) (<>)
  {-# INLINE (<.>) #-}
  (<. ) = coerce @(w -> w -> w) (<>)
  {-# INLINE (<. ) #-}
  ( .>) = coerce @(w -> w -> w) (<>)
  {-# INLINE ( .>) #-}
instance CApplicative []
instance CApplicative IO
instance CApplicative STM
instance CApplicative ReadP
instance CApplicative ReadPrec
instance CApplicative (SST.ST s)
instance CApplicative (LST.ST s)
instance CApplicative App.ZipList
instance CApplicative Maybe
instance CApplicative Identity
instance CApplicative Tree.Tree
instance CApplicative Seq.Seq
instance CApplicative Sem.Option
instance CApplicative NonEmpty
instance CApplicative ((->) a)
instance CApplicative (Either a)
instance (CApplicative f, CApplicative g)
      => CApplicative (SOP.Product f g) where
  pair (SOP.Pair a b) (SOP.Pair c d) = SOP.Pair (pair a c) (pair b d)
  SOP.Pair f g <.> SOP.Pair a b = SOP.Pair (f <.> a) (g <.> b)
  {-# INLINE (<.>) #-}
  SOP.Pair f g <. SOP.Pair a b = SOP.Pair (f <. a) (g <. b)
  {-# INLINE (<.) #-}
  SOP.Pair f g .> SOP.Pair a b = SOP.Pair (f .> a) (g .> b)
  {-# INLINE (.>) #-}

class Cat f (g a -> g b) => CatOver f g a b
instance Cat f (g a -> g b) => CatOver f g a b

instance Applicative f => CApplicative (WrapFunctor f)
instance Semigroup w => CApplicative ((,) w) where
  pair (w, a) (u, b) = (w <> u, (a, b))
  {-# INLINE pair #-}
  (w, f) <.> (u, a) = (w <> u, f a)
  {-# INLINE (<.>) #-}
  (w, a) <.  (u, _) = (w <> u, a)
  {-# INLINE (<.) #-}
  (w, _)  .> (u, b) = (w <> u, b)
  {-# INLINE (.>) #-}
instance CApplicative IM.IntMap where
  pair = IM.intersectionWith (,)
  {-# INLINE pair #-}
  (<.>) = IM.intersectionWith id
  {-# INLINE (<.>) #-}
  (<.)  = IM.intersectionWith const
  {-# INLINE (<.) #-}
  (.>)  = IM.intersectionWith $ const id
  {-# INLINE (.>) #-}

instance Ord k => CApplicative (Map.Map k) where
  pair = Map.intersectionWith (,)
  {-# INLINE pair #-}
  (<.>) = Map.intersectionWith id
  {-# INLINE (<.>) #-}
  (<.)  = Map.intersectionWith const
  {-# INLINE (<.) #-}
  (.>)  = Map.intersectionWith $ const id
  {-# INLINE (.>) #-}

instance (Eq k, Hashable k) => CApplicative (HM.HashMap k) where
  pair = HM.intersectionWith (,)
  {-# INLINE pair #-}
  (<.>) = HM.intersectionWith id
  {-# INLINE (<.>) #-}
  (<.)  = HM.intersectionWith const
  {-# INLINE (<.) #-}
  (.>)  = HM.intersectionWith $ const id
  {-# INLINE (.>) #-}

instance CApplicative Set.Set where
  pair as bs = foldMap (\b -> Set.map (,b) as) bs
  {-# INLINE pair #-}
  fs <.> as = foldMap (\f -> Set.map f as) fs
  {-# INLINE (<.>) #-}
  a <. b | Set.null b = Set.empty
         | otherwise  = a
  {-# INLINE (<.) #-}
  a .> b | Set.null a = Set.empty
         | otherwise  = b
  {-# INLINE (.>) #-}

instance CApplicative HS.HashSet where
  pair as bs = foldMap (\b -> HS.map (,b) as) bs
  {-# INLINE pair #-}
  fs <.> as = foldMap (\f -> HS.map f as) fs
  {-# INLINE (<.>) #-}
  a <. b | HS.null b = HS.empty
         | otherwise  = a
  {-# INLINE (<.) #-}
  a .> b | HS.null a = HS.empty
         | otherwise  = b
  {-# INLINE (.>) #-}

newtype CApp f a = CApp { runCApp :: f a }
  deriving (Read, Show, Eq, Ord)
  deriving newtype (Functor, Applicative, CFunctor, CApplicative)

instance (Cat f a, CApplicative f, Semigroup a, Cat f (a, a))
       => Semigroup (CApp f a) where
  CApp a <> CApp b = CApp $ uncurry (<>) <$:> pair a b
