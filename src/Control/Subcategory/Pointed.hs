{-# LANGUAGE TypeOperators #-}
module Control.Subcategory.Pointed where
import Control.Subcategory.Functor

import qualified Control.Applicative             as App
import qualified Control.Monad.ST.Lazy           as LST
import qualified Control.Monad.ST.Strict         as SST
import           Data.Default                    (Default)
import qualified Data.Functor.Compose            as SOP
import           Data.Functor.Identity           (Identity)
import qualified Data.Functor.Product            as SOP
import qualified Data.HashSet                    as HS
import qualified Data.IntSet                     as IS
import           Data.List.NonEmpty              (NonEmpty)
import qualified Data.Map                        as Map
import qualified Data.Monoid                     as Mon
import           Data.Ord                        (Down)
import qualified Data.Pointed                    as Pt
import           Data.Proxy                      (Proxy)
import qualified Data.Semigroup                  as Sem
import qualified Data.Sequence                   as Seq
import qualified Data.Set                        as Set
import qualified Data.Tree                       as Tree
import           GHC.Conc                        (STM)
import           GHC.Generics                    ((:*:) (..), (:.:) (..))
import           GHC.Generics                    (Par1, Rec1, U1)
import           Text.ParserCombinators.ReadP    (ReadP)
import           Text.ParserCombinators.ReadPrec (ReadPrec)

class CFunctor f => CPointed f where
  cpure :: Cat f a => a -> f a
  default cpure :: App.Applicative f => a -> f a
  cpure = pure

newtype WrapPointed f a = WrapApplicative { runWrapApplicative :: f a }
  deriving newtype (Functor, CFunctor, Applicative,
                    App.Alternative, Monad, Pt.Pointed)

instance (CFunctor f, Pt.Pointed f) => CPointed (WrapPointed f) where
  cpure = Pt.point
  {-# INLINE cpure #-}

instance CPointed []
instance CPointed Maybe
instance CPointed IO
instance CPointed (SST.ST s)
instance CPointed (LST.ST s)
instance CPointed Par1
instance CPointed Sem.Min
instance CPointed Sem.Max
instance CPointed Mon.First
instance CPointed Mon.Last
instance CPointed Sem.First
instance CPointed Sem.Last
instance CPointed Sem.Option
instance CPointed NonEmpty
instance CPointed App.ZipList
instance CPointed Identity
instance CPointed STM
instance CPointed Sem.Dual
instance CPointed Sem.Sum
instance CPointed Sem.Product
instance CPointed Down
instance CPointed Tree.Tree
instance CPointed Seq.Seq
instance (Ord k, Default k) => CPointed (Map.Map k) where
  cpure = Pt.point
  {-# INLINE cpure #-}
instance CPointed Set.Set where
  cpure = Pt.point
  {-# INLINE cpure #-}
instance CPointed (Either a)
instance CPointed U1
instance CPointed Proxy
instance Default m => CPointed (Sem.Arg m) where
  cpure = Pt.point
  {-# INLINE cpure #-}
instance (Functor f, Pt.Pointed f) => CPointed (Rec1 f) where
  cpure = Pt.point
  {-# INLINE cpure #-}
instance (Functor p, Functor q, Pt.Pointed p, Pt.Pointed q)
      => CPointed (p :*: q) where
  cpure a = (:*:) (Pt.point a) (Pt.point a)
  {-# INLINE cpure #-}
instance (Functor p, Functor q, Pt.Pointed p, Pt.Pointed q)
      => CPointed (p :.: q) where
  cpure a = Comp1 $ Pt.point $ Pt.point a
  {-# INLINE cpure #-}
instance (CFunctor p, CFunctor q, Pt.Pointed p, Pt.Pointed q)
      => CPointed (SOP.Compose p q) where
  cpure a = SOP.Compose $ Pt.point $ Pt.point a
  {-# INLINE cpure #-}
instance (CFunctor p, CFunctor q, CPointed p, CPointed q)
      => CPointed (SOP.Product p q) where
  cpure a = SOP.Pair (cpure a) (cpure a)
  {-# INLINE cpure #-}
instance CPointed ReadP
instance CPointed ReadPrec
instance CPointed (WrapIntContainer IS.IntSet) where
  cpure = WrapIntContainer . IS.singleton
  {-# INLINE cpure #-}
instance CPointed HS.HashSet where
  cpure = HS.singleton
  {-# INLINE cpure #-}
