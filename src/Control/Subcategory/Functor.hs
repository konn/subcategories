{-# LANGUAGE InstanceSigs, TypeOperators #-}
module Control.Subcategory.Functor
  ( CFunctor(..), (<$:>)
  , defaultEmapConst, WrapFunctor(..), WrapMono(..)
  , WrapIntContainer(..)
  ) where
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
import qualified Data.IntSet                     as IS
import           Data.Kind                       (Constraint)
import           Data.List.NonEmpty              (NonEmpty)
import qualified Data.Map                        as Map
import qualified Data.Monoid                     as Mon
import           Data.MonoTraversable            (Element, MonoFunctor (..))
import           Data.Ord                        (Down (..))
import           Data.Proxy                      (Proxy)
import           Data.Semigroup                  (Semigroup ((<>)))
import qualified Data.Semigroup                  as Sem
import qualified Data.Sequence                   as Seq
import qualified Data.Set                        as Set
import qualified Data.Tree                       as Tree
import           Foreign.Ptr                     (Ptr)
import           GHC.Conc                        (STM)
import           GHC.Generics                    ((:*:), (:+:), (:.:), K1, M1)
import           GHC.Generics                    (Par1, Rec1, U1, URec, V1)
import qualified System.Console.GetOpt           as GetOpt
import           Text.ParserCombinators.ReadP    (ReadP)
import           Text.ParserCombinators.ReadPrec (ReadPrec)

infixl 4 <$:

class CFunctor f where
  type Cat f a :: Constraint
  type Cat f a = ()

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

newtype WrapFunctor f a = WrapFunctor { runFunctor :: f a }
  deriving newtype (Functor, Applicative, App.Alternative, Monad)

instance Functor f => CFunctor (WrapFunctor f) where
  type Cat (WrapFunctor f) a = ()
  emap :: forall a b. (a -> b) -> WrapFunctor f a -> WrapFunctor f b
  emap = fmap
  {-# INLINE emap #-}
  (<$:) :: forall a b. a -> WrapFunctor f b -> WrapFunctor f a
  (<$:) = (<$)
  {-# INLINE (<$:) #-}

instance CFunctor []
instance CFunctor Maybe
instance CFunctor IO
instance CFunctor Par1
instance CFunctor NonEmpty
instance CFunctor ReadP
instance CFunctor ReadPrec
instance CFunctor Down where
  emap f (Down a) = Down $ f a
  {-# INLINE emap #-}
  (<$:) = defaultEmapConst
  {-# INLINE (<$:) #-}
instance CFunctor Mon.Product
instance CFunctor Mon.Sum
instance CFunctor Mon.Dual
instance CFunctor Mon.Last
instance CFunctor Mon.First
instance CFunctor STM
instance CFunctor Handler
instance CFunctor Identity
instance CFunctor App.ZipList
instance CFunctor GetOpt.ArgDescr
instance CFunctor GetOpt.OptDescr
instance CFunctor GetOpt.ArgOrder
instance CFunctor Sem.Option
instance CFunctor Sem.Last
instance CFunctor Sem.First
instance CFunctor Sem.Max
instance CFunctor Sem.Min
instance CFunctor Complex
instance CFunctor (Either a)
instance CFunctor V1
instance CFunctor U1
instance CFunctor ((,) a)
instance CFunctor (SST.ST s)
instance CFunctor (LST.ST s)
instance CFunctor Proxy
instance Arrow a => CFunctor (ArrowMonad a)
instance Monad m => CFunctor (App.WrappedMonad m)
instance CFunctor (Sem.Arg a)
instance Functor f => CFunctor (Rec1 f)
instance CFunctor (URec Char)
instance CFunctor (URec Double)
instance CFunctor (URec Float)
instance CFunctor (URec Int)
instance CFunctor (URec Word)
instance CFunctor (URec (Ptr ()))
instance CFunctor f => CFunctor (Mon.Alt f) where
  type Cat (Mon.Alt f) a = Cat f a
  emap f = Mon.Alt . emap f . Mon.getAlt
  {-# INLINE emap #-}
  (<$:) = defaultEmapConst
  {-# INLINE (<$:) #-}
instance CFunctor (Const m)
instance Arrow a => CFunctor (App.WrappedArrow a b)
instance CFunctor ((->) r)
instance CFunctor (K1 i c)
instance (Functor f, Functor g) => CFunctor (f :+: g)
instance (Functor f, Functor g) => CFunctor (f :*: g)
instance (Functor f, Functor g) => CFunctor (f :.: g)
instance (CFunctor f, CFunctor g) => CFunctor (SOP.Sum f g) where
  type Cat (SOP.Sum f g) a = (Cat f a, Cat g a)
  emap f (SOP.InL a) = SOP.InL $ emap f a
  emap f (SOP.InR b) = SOP.InR $ emap f b
  {-# INLINE emap #-}
  (<$:) = defaultEmapConst
  {-# INLINE (<$:) #-}
instance (CFunctor f, CFunctor g) => CFunctor (SOP.Product f g) where
  type Cat (SOP.Product f g) a = (Cat f a, Cat g a)
  emap f (SOP.Pair a b) = SOP.Pair (emap f a) (emap f b)
  {-# INLINE emap #-}
  (<$:) = defaultEmapConst
  {-# INLINE (<$:) #-}
instance (CFunctor f, CFunctor g) => CFunctor (SOP.Compose f g) where
  type Cat (SOP.Compose f g) a = (Cat g a, Cat f (g a))
  emap f (SOP.Compose a) = SOP.Compose $ emap (emap f) a
  (<$:) = defaultEmapConst
  {-# INLINE (<$:) #-}
instance Functor f => CFunctor (M1 i c f)
instance CFunctor Seq.Seq

data WrapMono a b where
  WrapMono :: {unwrapMono :: {-# UNPACK #-}!a} -> WrapMono a (Element a)

instance {-# OVERLAPPABLE #-}  MonoFunctor a => CFunctor (WrapMono a) where
  type Cat (WrapMono a) b = b ~ Element a
  emap f = WrapMono . omap f . unwrapMono
  {-# INLINE emap #-}
  (<$:) = defaultEmapConst
  {-# INLINE (<$:) #-}

data WrapIntContainer a b where
  WrapIntContainer :: {unwrapIntContainer :: {-# UNPACK #-} !a } -> WrapIntContainer a (Element a)

instance (b ~ Element a, Semigroup a) => Semigroup (WrapIntContainer a b) where
  WrapIntContainer a <> WrapIntContainer b = WrapIntContainer $ a <> b

instance (b ~ Element a, Monoid a) => Monoid (WrapIntContainer a b) where
  mempty = WrapIntContainer mempty
  {-# INLINE mempty #-}
  mappend (WrapIntContainer a) (WrapIntContainer b) =
    WrapIntContainer $ a `mappend` b
  {-# INLINE mappend #-}

instance CFunctor (WrapIntContainer IS.IntSet) where
  type Cat (WrapIntContainer IS.IntSet) b = b ~ Int
  emap f = WrapIntContainer . IS.map f . unwrapIntContainer
  {-# INLINE emap #-}
  (<$:) = defaultEmapConst
  {-# INLINE (<$:) #-}

instance CFunctor IM.IntMap
instance Ord k => CFunctor (Map.Map k)

instance CFunctor Set.Set where
  type Cat Set.Set a = Ord a
  emap = Set.map
  {-# INLINE emap #-}
  (<$:) = defaultEmapConst
  {-# INLINE (<$:) #-}

instance CFunctor HS.HashSet where
  type Cat HS.HashSet a = (Hashable a, Eq a)
  emap = HS.map
  {-# INLINE emap #-}
  (<$:) = defaultEmapConst
  {-# INLINE (<$:) #-}

instance CFunctor (HM.HashMap k)
instance CFunctor Tree.Tree

infixl 4 <$:>
(<$:>) :: (CFunctor f, Cat f a, Cat f b) => (a -> b) -> f a -> f b
(<$:>) = emap
{-# INLINE (<$:>) #-}
