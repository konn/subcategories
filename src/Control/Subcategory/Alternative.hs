{-# LANGUAGE EmptyCase, ScopedTypeVariables, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Control.Subcategory.Alternative
  (CAlternative(..), CChoice(..), CAlt(..)) where
import Control.Subcategory.Alternative.Class
import Control.Subcategory.Applicative.Class
import Control.Subcategory.Functor
import Control.Subcategory.Pointed

import qualified Control.Applicative             as App
import           Data.Coerce                     (coerce)
import qualified Data.Functor.Compose            as SOP
import qualified Data.Functor.Product            as SOP
import           Data.Hashable                   (Hashable)
import qualified Data.HashMap.Strict             as HM
import qualified Data.HashSet                    as HS
import qualified Data.IntMap                     as IM
import           Data.List.NonEmpty              (NonEmpty)
import qualified Data.Map                        as Map
import           Data.MonoTraversable            (GrowingAppend, MonoFunctor)
import qualified Data.Primitive.Array            as A
import qualified Data.Primitive.PrimArray        as PA
import qualified Data.Primitive.SmallArray       as SA
import qualified Data.Semigroup                  as Sem
import qualified Data.Sequence                   as Seq
import qualified Data.Set                        as Set
import qualified Data.Vector                     as V
import qualified Data.Vector.Primitive           as P
import qualified Data.Vector.Storable            as S
import qualified Data.Vector.Unboxed             as U
import           Text.ParserCombinators.ReadP    (ReadP)
import           Text.ParserCombinators.ReadPrec (ReadPrec)

instance CChoice []
instance CChoice Maybe
instance CChoice V.Vector
instance CChoice U.Vector where
  (<!>) = (<>)
  {-# INLINE [1] (<!>) #-}
instance CChoice S.Vector where
  (<!>) = (<>)
  {-# INLINE [1] (<!>) #-}
instance CChoice P.Vector where
  (<!>) = (<>)
  {-# INLINE [1] (<!>) #-}
instance CChoice PA.PrimArray where
  (<!>) = (<>)
  {-# INLINE [1] (<!>) #-}
instance CChoice SA.SmallArray
instance CChoice A.Array
instance CChoice Seq.Seq
instance CChoice Sem.Option
instance CChoice NonEmpty where
  (<!>) = (Sem.<>)
  {-# INLINE (<!>) #-}
instance CChoice (Either a) where
  Left _ <!> b = b
  a      <!> _ = a
  {-# INLINE (<!>) #-}
instance CChoice IM.IntMap where
  (<!>) = IM.union
instance CChoice ReadP
instance CChoice ReadPrec
instance (CChoice f, CFunctor g) => CChoice (SOP.Compose f g) where
  SOP.Compose a <!> SOP.Compose b = SOP.Compose (a <!> b)
  {-# INLINE (<!>) #-}

instance (CChoice f, CChoice g) => CChoice (SOP.Product f g) where
  SOP.Pair a1 b1 <!> SOP.Pair a2 b2 =
    SOP.Pair (a1 <!> a2) (b1 <!> b2)
  {-# INLINE (<!>) #-}

instance CChoice HS.HashSet where
  (<!>) = HS.union
  {-# INLINE (<!>) #-}

instance CChoice Set.Set where
  (<!>) = Set.union
  {-# INLINE (<!>) #-}

instance Ord k => CChoice (Map.Map k) where
  (<!>) = Map.union
  {-# INLINE (<!>) #-}

instance
    (MonoFunctor mono, GrowingAppend mono, Semigroup mono)
  => CChoice (WrapMono mono) where
  (<!>) = (<>)
  {-# INLINE [1] (<!>) #-}

instance (Eq k, Hashable k) => CChoice (HM.HashMap k) where
  (<!>) = HM.union
  {-# INLINE (<!>) #-}

instance CAlternative IM.IntMap where
  cempty = IM.empty
  {-# INLINE cempty #-}
instance (Eq k, Hashable k) => CAlternative (HM.HashMap k) where
  cempty = HM.empty
  {-# INLINE cempty #-}
instance Ord k => CAlternative (Map.Map k) where
  cempty = Map.empty
  {-# INLINE cempty #-}
instance CAlternative HS.HashSet where
  cempty = HS.empty
  {-# INLINE cempty #-}
instance CAlternative Set.Set where
  cempty = Set.empty
  {-# INLINE cempty #-}
instance (MonoFunctor mono, Monoid mono, GrowingAppend mono)
      => CAlternative (WrapMono mono) where
  cempty = WrapMono mempty
  {-# INLINE [1] cempty #-}

instance (CAlternative f, CFunctor g) => CAlternative (SOP.Compose f g) where
  cempty = SOP.Compose cempty
  {-# INLINE cempty #-}

instance (CAlternative f, CAlternative g) => CAlternative (SOP.Product f g) where
  cempty = SOP.Pair cempty cempty
  {-# INLINE cempty #-}

instance CAlternative []
instance CAlternative Maybe
instance CAlternative Seq.Seq
instance CAlternative Sem.Option
instance CAlternative ReadP
instance CAlternative V.Vector
instance CAlternative U.Vector where
  cempty = U.empty
  {-# INLINE [1] cempty #-}
instance CAlternative S.Vector where
  cempty = S.empty
  {-# INLINE [1] cempty #-}
instance CAlternative P.Vector where
  cempty = P.empty
  {-# INLINE [1] cempty #-}
instance CAlternative PA.PrimArray where
  cempty = PA.primArrayFromListN 0 []
  {-# INLINE [1] cempty #-}
instance CAlternative SA.SmallArray
instance CAlternative A.Array
instance CAlternative ReadPrec

newtype CAlt f a = CAlt { runAlt :: f a }
  deriving newtype (Functor, Constrained, Applicative, App.Alternative)
deriving newtype instance CFunctor f => CFunctor (CAlt f)
deriving newtype instance CChoice f => CChoice (CAlt f)
deriving newtype instance CAlternative f => CAlternative (CAlt f)
deriving newtype instance CApplicative f => CApplicative (CAlt f)
deriving newtype instance CPointed f => CPointed (CAlt f)


instance (Dom f a, CChoice f) => Sem.Semigroup (CAlt f a) where
  (<>) = coerce @(f a -> f a -> f a) (<!>)

instance (Dom f a, CAlternative f) => Monoid (CAlt f a) where
  mempty = coerce @(f a) cempty
  mappend = coerce @(f a -> f a -> f a) (<!>)
