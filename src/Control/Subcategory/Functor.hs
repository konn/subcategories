{-# LANGUAGE GADTs, InstanceSigs, KindSignatures, PatternSynonyms           #-}
{-# LANGUAGE RankNTypes, RoleAnnotations, ScopedTypeVariables               #-}
{-# LANGUAGE TemplateHaskell, TypeApplications, TypeFamilies, TypeOperators #-}
module Control.Subcategory.Functor
  ( Constrained(..), CFunctor (..),
    (<$:>),
    defaultEmapConst,
    WrapFunctor (..),
    WrapMono (WrapMono, unwrapMono),
  )
where
import qualified Control.Applicative             as App
import           Control.Arrow                   (Arrow, ArrowMonad)
import           Control.Exception               (Handler)
import qualified Control.Monad.ST.Lazy           as LST
import qualified Control.Monad.ST.Strict         as SST
import           Data.Complex                    (Complex)
import qualified Data.Functor.Compose            as SOP
import           Data.Functor.Const              (Const)
import           Data.Functor.Identity           (Identity)
import qualified Data.Functor.Product            as SOP
import qualified Data.Functor.Sum                as SOP
import           Data.Hashable                   (Hashable)
import qualified Data.HashMap.Strict             as HM
import qualified Data.HashSet                    as HS
import qualified Data.IntMap                     as IM
import           Data.Kind                       (Constraint, Type)
import           Data.List.NonEmpty              (NonEmpty)
import qualified Data.Map                        as Map
import qualified Data.Monoid                     as Mon
import           Data.MonoTraversable            (Element, MonoFunctor (..),
                                                  WrappedMono)
import           Data.Ord                        (Down (..))
import           Data.Proxy                      (Proxy)
import qualified Data.Semigroup                  as Sem
import qualified Data.Sequence                   as Seq
import qualified Data.Set                        as Set
import qualified Data.Tree                       as Tree
import           Foreign.Ptr                     (Ptr)
import           GHC.Conc                        (STM)
import           GHC.Generics                    ((:*:), (:+:), (:.:), K1, M1,
                                                  Par1, Rec1, U1, URec, V1)
import qualified System.Console.GetOpt           as GetOpt
import           Text.ParserCombinators.ReadP    (ReadP)
import           Text.ParserCombinators.ReadPrec (ReadPrec)


infixl 4 <$:

class Constrained (f :: Type -> Type) where
  type Cat f (a :: k) :: Constraint
  type Cat f a = ()

class Constrained f => CFunctor f where
  emap :: (Cat f a, Cat f b) => (a -> b) -> f a -> f b
  default emap :: Functor f => (a -> b) -> f a -> f b
  emap = fmap
  {-# INLINE emap #-}
  (<$:) :: (Cat f a, Cat f b) => a -> f b -> f a
  default (<$:) :: Functor f => a -> f b -> f a
  (<$:) = (<$)
  {-# INLINE (<$:) #-}

defaultEmapConst :: (CFunctor f, Cat f a, Cat f b) => a -> f b -> f a
defaultEmapConst = emap . const
{-# INLINE defaultEmapConst #-}

newtype WrapFunctor f a = WrapFunctor {runFunctor :: f a}
  deriving newtype (Functor, Applicative, App.Alternative, Monad)

instance Constrained (WrapFunctor f) where
  type Cat (WrapFunctor f) a = ()

instance Functor f => CFunctor (WrapFunctor f) where
  emap :: (a -> b) -> WrapFunctor f a -> WrapFunctor f b
  emap = fmap
  {-# INLINE emap #-}
  (<$:) :: a -> WrapFunctor f b -> WrapFunctor f a
  (<$:) = (<$)
  {-# INLINE (<$:) #-}

instance Constrained []
instance CFunctor []
instance Constrained Maybe
instance CFunctor Maybe
instance Constrained IO
instance CFunctor IO
instance Constrained Par1
instance CFunctor Par1
instance Constrained NonEmpty
instance CFunctor NonEmpty
instance Constrained ReadP
instance CFunctor ReadP
instance Constrained ReadPrec
instance CFunctor ReadPrec


instance Constrained Down
instance CFunctor Down
instance Constrained Mon.Product
instance CFunctor Mon.Product

instance Constrained Mon.Sum
instance CFunctor Mon.Sum
instance Constrained Mon.Dual
instance CFunctor Mon.Dual

instance Constrained Mon.Last
instance CFunctor Mon.Last
instance Constrained Mon.First
instance CFunctor Mon.First

instance Constrained STM
instance CFunctor STM
instance Constrained Handler
instance CFunctor Handler

instance Constrained Identity
instance CFunctor Identity
instance Constrained App.ZipList
instance CFunctor App.ZipList
instance Constrained GetOpt.ArgDescr
instance CFunctor GetOpt.ArgDescr
instance Constrained GetOpt.OptDescr
instance CFunctor GetOpt.OptDescr
instance Constrained GetOpt.ArgOrder
instance CFunctor GetOpt.ArgOrder
instance Constrained Sem.Option
instance CFunctor Sem.Option

instance Constrained Sem.Last
instance CFunctor Sem.Last
instance Constrained Sem.First
instance CFunctor Sem.First

instance Constrained Sem.Max
instance CFunctor Sem.Max
instance Constrained Sem.Min
instance CFunctor Sem.Min

instance Constrained Complex
instance CFunctor Complex
instance Constrained (Either a)
instance CFunctor (Either a)

instance Constrained V1
instance CFunctor V1
instance Constrained U1
instance CFunctor U1

instance Constrained ((,) a)
instance CFunctor ((,) a)
instance Constrained (SST.ST s)
instance CFunctor (SST.ST s)

instance Constrained (LST.ST s)
instance CFunctor (LST.ST s)
instance Constrained Proxy
instance CFunctor Proxy

instance Constrained (ArrowMonad a)
instance Arrow a => CFunctor (ArrowMonad a)
instance Constrained (App.WrappedMonad m)
instance Monad m => CFunctor (App.WrappedMonad m)

instance Constrained (Sem.Arg a)
instance CFunctor (Sem.Arg a)
instance Constrained (Rec1 f)
instance Functor f => CFunctor (Rec1 f)

instance Constrained (URec Char)
instance CFunctor (URec Char)
instance Constrained (URec Double)
instance CFunctor (URec Double)

instance Constrained (URec Float)
instance CFunctor (URec Float)
instance Constrained (URec Int)
instance CFunctor (URec Int)

instance Constrained (URec Word)
instance CFunctor (URec Word)
instance Constrained (URec (Ptr ()))
instance CFunctor (URec (Ptr ()))

instance Constrained (Mon.Alt f) where
  type Cat (Mon.Alt f) a = Cat f a

instance CFunctor f => CFunctor (Mon.Alt f) where

  emap f = Mon.Alt . emap f . Mon.getAlt
  {-# INLINE emap #-}

  (<$:) = defaultEmapConst
  {-# INLINE (<$:) #-}

instance Constrained (Const m)
instance CFunctor (Const m)
instance Constrained (App.WrappedArrow a b)
instance Arrow a => CFunctor (App.WrappedArrow a b)

instance Constrained ((->) r)
instance CFunctor ((->) r)
instance Constrained (K1 i c)
instance CFunctor (K1 i c)

instance Constrained (f :+: g)
instance (Functor f, Functor g) => CFunctor (f :+: g)
instance Constrained (f :*: g)
instance (Functor f, Functor g) => CFunctor (f :*: g)

instance Constrained (f :.: g)
instance (Functor f, Functor g) => CFunctor (f :.: g)
instance (Constrained f, Constrained g) => Constrained (SOP.Sum f g) where
  type Cat (SOP.Sum f g) a = (Cat f a, Cat g a)

instance (CFunctor f, CFunctor g) => CFunctor (SOP.Sum f g) where
  emap f (SOP.InL a) = SOP.InL $ emap f a

  emap f (SOP.InR b) = SOP.InR $ emap f b
  {-# INLINE emap #-}

  (<$:) = defaultEmapConst
  {-# INLINE (<$:) #-}

instance (Constrained f, Constrained g) => Constrained (SOP.Product f g) where
  type Cat (SOP.Product f g) a = (Cat f a, Cat g a)

instance (CFunctor f, CFunctor g) => CFunctor (SOP.Product f g) where
  emap f (SOP.Pair a b) = SOP.Pair (emap f a) (emap f b)
  {-# INLINE emap #-}

  (<$:) = defaultEmapConst
  {-# INLINE (<$:) #-}

instance (Constrained (f ::Type -> Type), Constrained (g :: Type -> Type))
  => Constrained (SOP.Compose f g) where
  type Cat (SOP.Compose f g) a = (Cat g a, Cat f (g a))

instance (CFunctor f, CFunctor g) => CFunctor (SOP.Compose f g) where
  emap f (SOP.Compose a) = SOP.Compose $ emap (emap f) a
  (<$:) = defaultEmapConst

  {-# INLINE (<$:) #-}

instance Constrained (M1 i c f)
instance Functor f => CFunctor (M1 i c f)

instance Constrained Seq.Seq
instance CFunctor Seq.Seq

type role WrapMono representational nominal
-- | Similar to 'WrappedMono' from @mono-traversable,
--   but uses @newtype@ instaed of GADTs, which is efficient.
--   To restrict the construction, we hide genuine constructor
--   and expose the constrained pattern synonym 'WrapMono' and
--   specifies type roles tightly (note: the role for @mono@
--   should NOT be representational honestly; indeed, @WrapMono mono a@
--   could be coerced to @WrapMono mono' a@ iff @mono@ and @mono' are
--   representationally equivalent __AND__ @Element a ~ Element a@.)
newtype WrapMono mono b = WrapMono' mono

pattern WrapMono :: b ~ Element mono => b ~ Element mono => mono -> WrapMono mono b
pattern WrapMono {unwrapMono} = WrapMono' unwrapMono

instance Constrained (WrappedMono mono) where
  type Cat (WrappedMono mono) a = a ~ Element mono

instance Constrained (WrapMono mono) where
  type Cat (WrapMono mono) b = b ~ Element mono

instance {-# OVERLAPPABLE #-} MonoFunctor a
      => CFunctor (WrapMono a) where
  emap f = WrapMono . omap f . unwrapMono

  {-# INLINE emap #-}
  (<$:) = defaultEmapConst

  {-# INLINE (<$:) #-}


instance Constrained IM.IntMap
instance CFunctor IM.IntMap

instance Constrained (Map.Map k)
instance Ord k => CFunctor (Map.Map k)

instance Constrained Set.Set where
  type Cat Set.Set a = Ord a

instance CFunctor Set.Set where
  emap = Set.map
  {-# INLINE emap #-}
  (<$:) = defaultEmapConst
  {-# INLINE (<$:) #-}

instance Constrained HS.HashSet where
  type Cat HS.HashSet a = (Hashable a, Eq a)

instance CFunctor HS.HashSet where
  emap = HS.map
  {-# INLINE emap #-}
  (<$:) = defaultEmapConst
  {-# INLINE (<$:) #-}

instance Constrained (HM.HashMap k)
instance CFunctor (HM.HashMap k)
instance Constrained Tree.Tree
instance CFunctor Tree.Tree


infixl 4 <$:>
(<$:>) :: (CFunctor f, Cat f a, Cat f b) => (a -> b) -> f a -> f b
(<$:>) = emap
{-# INLINE (<$:>) #-}
