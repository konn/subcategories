{-# LANGUAGE CPP, DerivingVia, StandaloneDeriving, TypeOperators #-}
module Control.Subcategory.Zip
  ( CZip(..),
    CZippy(..),
    CRepeat(..),
    module Control.Subcategory.Semialign
  )where
import           Control.Applicative           (ZipList (..))
import           Control.Subcategory.Functor
import           Control.Subcategory.Semialign
import           Data.Coerce                   (coerce)
import           Data.Functor.Compose          (Compose (..))
import           Data.Functor.Identity
import qualified Data.Functor.Product          as SOP
import           Data.Hashable                 (Hashable)
import qualified Data.HashMap.Strict           as HM
import qualified Data.List.NonEmpty            as NE
import qualified Data.Map.Strict               as M
import           Data.Proxy
import           Data.Semigroup                (Option (..))
import qualified Data.Sequence                 as Seq
import           Data.Tree
import qualified Data.Vector                   as V
import qualified Data.Vector.Primitive         as Prim
import qualified Data.Vector.Storable          as S
import qualified Data.Vector.Unboxed           as U
import           Data.Zip
import           GHC.Generics                  ((:*:) (..), (:.:) (..))
import           Prelude                       hiding (repeat, zip, zipWith)
import qualified Prelude                       as P

class CSemialign f => CZip f where
  czipWith
    :: (Dom f a, Dom f b, Dom f c)
    => (a -> b -> c) -> f a -> f b -> f c
  czip
    :: (Dom f a, Dom f b, Dom f (a, b))
    => f a -> f b -> f (a, b)
  {-# INLINE [1] czip #-}
  czip = czipWith (,)

instance Zip f => CZip (WrapFunctor f) where
  czip = zip
  {-# INLINE [1] czip #-}
  czipWith = zipWith
  {-# INLINE [1] czipWith #-}

deriving via WrapFunctor [] instance CZip []
deriving via WrapFunctor Maybe instance CZip Maybe
deriving newtype instance CZip Option
deriving via WrapFunctor ZipList instance CZip ZipList
deriving via WrapFunctor Identity instance CZip Identity
deriving via WrapFunctor NE.NonEmpty instance CZip NE.NonEmpty
deriving via WrapFunctor Tree instance CZip Tree
deriving via WrapFunctor Seq.Seq instance CZip Seq.Seq
deriving via WrapFunctor (M.Map k) instance Ord k => CZip (M.Map k)
deriving via WrapFunctor (HM.HashMap k)
  instance (Eq k, Hashable k)
  => CZip (HM.HashMap k)
deriving via WrapFunctor ((->) e) instance CZip ((->) e)

instance CZip V.Vector where
  czip = V.zip
  {-# INLINE [1] czip #-}
  czipWith = V.zipWith
  {-# INLINE [1] czipWith #-}

instance CZip U.Vector where
  czip = U.zip
  {-# INLINE [1] czip #-}
  czipWith = U.zipWith
  {-# INLINE [1] czipWith #-}

instance CZip S.Vector where
  czipWith = S.zipWith
  {-# INLINE [1] czipWith #-}

instance CZip Prim.Vector where
  czipWith = Prim.zipWith
  {-# INLINE [1] czipWith #-}

instance CZip Proxy where
  czip = const $ const Proxy
  {-# INLINE czip #-}
  czipWith = const $ const $ const Proxy
  {-# INLINE czipWith #-}

instance (CZip f, CZip g) => CZip (SOP.Product f g) where
  czipWith f (SOP.Pair a b) (SOP.Pair c d) =
    SOP.Pair (czipWith f a c) (czipWith f b d)
  {-# INLINE [1] czipWith #-}
  czip (SOP.Pair a b) (SOP.Pair c d) =
    SOP.Pair (czip a c) (czip b d)
  {-# INLINE [1] czip #-}

instance (CZip f, CZip g) => CZip (f :*: g) where
  czipWith f (a :*: b) (c :*: d) =
    czipWith f a c :*: czipWith f b d
  {-# INLINE [1] czipWith #-}
  czip (a :*: b) (c :*: d) =
    czip a c :*: czip b d
  {-# INLINE [1] czip #-}

instance (CZip f, CZip g) => CZip (Compose f g) where
  czipWith f (Compose a) (Compose b) =
    Compose $ czipWith (czipWith f) a b
  {-# INLINE [1] czipWith #-}
  czip (Compose a) (Compose b) =
    Compose $ czipWith czip a b
  {-# INLINE [1] czip #-}

instance (CZip f, CZip g) => CZip (f :.: g) where
  czipWith f (Comp1 a) (Comp1 b) =
    Comp1 $ czipWith (czipWith f) a b
  {-# INLINE [1] czipWith #-}
  czip (Comp1 a) (Comp1 b) =
    Comp1 $ czipWith czip a b
  {-# INLINE [1] czip #-}

{-# RULES
"czip/List"
  czip = P.zip
"czipWith/List"
  czipWith = P.zipWith
"czip/NonEmpty"
  czip = NE.zip
"czipWith/NonEmpty"
  czipWith = NE.zipWith
"czip/Seq"
  czip = Seq.zip
"czipWith/Seq"
  czipWith = Seq.zipWith
  #-}

class CZip f => CRepeat f where
  crepeat :: Dom f a => a -> f a

newtype CZippy f a = CZippy { runCZippy :: f a }
  deriving (Show, Read)
  deriving newtype (Functor, Zip, Semialign, Eq, Ord)
  deriving newtype (Constrained)
#if MIN_VERSION_semialign(1,1,0)
  deriving newtype Repeat
#endif

instance CFunctor f => CFunctor (CZippy f) where
  cmap = coerce $ cmap @f @a @b
    :: forall a b. (Dom f a, Dom f b) => (a -> b) -> CZippy f a -> CZippy f b
  {-# INLINE [1] cmap #-}

instance CSemialign f => CSemialign (CZippy f) where
  calignWith = \f -> coerce $ calignWith @f f
  {-# INLINE [1] calignWith #-}

instance CZip f => CZip (CZippy f) where
  czipWith f = coerce $ czipWith @f f
  {-# INLINE [1] czipWith #-}

instance CRepeat f => CRepeat (CZippy f) where
  crepeat = CZippy . crepeat
  {-# INLINE [1] crepeat #-}

instance (CZip f, Dom f a, Semigroup a) => Semigroup (CZippy f a) where
  (<>) = coerce $ czipWith @f ((<>) @a)
  {-# INLINE [1] (<>) #-}

instance (CRepeat f, Dom f a, Monoid a) => Monoid (CZippy f a) where
  mempty = coerce $ crepeat @f (mempty @a)
  {-# INLINE [1] mempty #-}

#if MIN_VERSION_semialign(1,1,0)
instance Repeat f => CRepeat (WrapFunctor f) where
  crepeat = coerce $ repeat @f @a
    :: forall a. a -> WrapFunctor f a
  {-# INLINE [1] crepeat #-}
deriving via WrapFunctor [] instance CRepeat []
deriving via WrapFunctor Maybe instance CRepeat Maybe
deriving newtype instance CRepeat Option
deriving via WrapFunctor ZipList instance CRepeat ZipList
deriving via WrapFunctor Identity instance CRepeat Identity
deriving via WrapFunctor NE.NonEmpty instance CRepeat NE.NonEmpty
deriving via WrapFunctor Tree instance CRepeat Tree
deriving via WrapFunctor ((->) e) instance CRepeat ((->) e)
#else
instance CRepeat [] where
  crepeat = P.repeat
  {-# INLINE [1] crepeat #-}
instance CRepeat Maybe where
  crepeat = Just
  {-# INLINE [1] crepeat #-}
deriving newtype instance CRepeat Option
deriving newtype instance CRepeat ZipList
instance CRepeat Identity where
  crepeat = Identity
  {-# INLINE [1] crepeat #-}
instance CRepeat NE.NonEmpty where
  crepeat = NE.repeat
  {-# INLINE [1] crepeat #-}
instance CRepeat Tree where
  crepeat x = n where n = Node x (repeat n)
  {-# INLINE [1] crepeat #-}
instance CRepeat Proxy where
  crepeat = const Proxy
  {-# INLINE [1] crepeat #-}
instance CRepeat ((->) e) where
  crepeat = const
  {-# INLINE [1] crepeat #-}
#endif
