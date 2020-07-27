{-# LANGUAGE DerivingVia, StandaloneDeriving, TypeOperators #-}
module Control.Subcategory.Zip where
import           Control.Applicative           (ZipList (ZipList))
import           Control.Subcategory.Functor
import           Control.Subcategory.Semialign
import           Data.Data                     (Proxy (Proxy))
import           Data.Functor.Compose          (Compose (..))
import           Data.Functor.Identity         (Identity)
import qualified Data.Functor.Product          as SOP
import           Data.Hashable                 (Hashable)
import qualified Data.HashMap.Strict           as HM
import qualified Data.List.NonEmpty            as NE
import qualified Data.Map.Strict               as M
import qualified Data.Sequence                 as Seq
import           Data.Tree                     (Tree)
import qualified Data.Vector                   as V
import qualified Data.Vector.Primitive         as Prim
import qualified Data.Vector.Storable          as S
import qualified Data.Vector.Unboxed           as U
import           Data.Zip
import           GHC.Generics                  ((:*:) (..), (:.:) (..))
import           Prelude                       hiding (zip, zipWith)
import qualified Prelude                       as P

class CSemialign f => CZip f where
  czipWith
    :: (Cat f a, Cat f b, Cat f c)
    => (a -> b -> c) -> f a -> f b -> f c
  czip
    :: (Cat f a, Cat f b, Cat f (a, b))
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