{-# LANGUAGE CPP, DerivingStrategies, DerivingVia                          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, TypeOperators #-}
module Control.Subcategory.Semialign
  ( CSemialign(..), CAlign(..)
  ) where
import           Control.Applicative         (ZipList)
import           Control.Subcategory.Functor
import           Data.Coerce
import           Data.Functor.Compose        (Compose (..))
import           Data.Functor.Identity       (Identity)
import qualified Data.Functor.Product        as SOP
import           Data.Hashable               (Hashable)
import           Data.HashMap.Strict         (HashMap)
import           Data.IntMap.Strict          (IntMap)
import qualified Data.IntSet                 as IS
import           Data.List.NonEmpty          (NonEmpty)
import           Data.Map                    (Map)
import           Data.Proxy                  (Proxy)
import           Data.Semialign
import           Data.Semigroup              (Option)
import           Data.Sequence               (Seq)
import           Data.These                  (These (..))
import           Data.Tree                   (Tree)
import qualified Data.Vector                 as V
import qualified Data.Vector.Primitive       as P
import qualified Data.Vector.Storable        as S
import qualified Data.Vector.Unboxed         as U
import           GHC.Generics                ((:*:) (..), (:.:) (..))

class CFunctor f => CSemialign f where
  {-# MINIMAL calignWith #-}
  calignWith
    :: (Dom f a, Dom f b, Dom f c)
    => (These a b -> c) -> f a -> f b -> f c
  calign
    :: (Dom f a, Dom f b, Dom f (These a b))
    => f a -> f b -> f (These a b)
  {-# INLINE [1] calign #-}
  calign = calignWith id

instance Semialign f => CSemialign (WrapFunctor f) where
  calignWith = alignWith
  {-# INLINE [1] calignWith #-}
  calign = align
  {-# INLINE [1] calign #-}

instance CSemialign (WrapMono IS.IntSet) where
  calignWith f = withMonoCoercible @IS.IntSet $
    coerce @(IS.IntSet -> IS.IntSet -> IS.IntSet) $ \ l r ->
    let ints = l `IS.intersection` r
    in IS.unions
          [ IS.map (f . This) l
          , IS.map (f . That) r
          , IS.map (\x -> f $ These x x) ints
          ]
  {-# INLINE [1] calignWith #-}

class CSemialign f => CAlign f where
  cnil :: Dom f a => f a

instance Align f => CAlign (WrapFunctor f) where
  cnil = WrapFunctor nil
  {-# INLINE [1] cnil #-}

deriving via WrapFunctor [] instance CSemialign []
deriving via WrapFunctor [] instance CAlign []
deriving via WrapFunctor Maybe instance CSemialign Maybe
deriving via WrapFunctor Maybe instance CAlign Maybe
#if MIN_VERSION_semialign(1,1,0)
deriving via WrapFunctor Option instance CSemialign Option
deriving via WrapFunctor Option instance CAlign Option
#else
deriving newtype instance CSemialign Option
deriving newtype instance CAlign Option
#endif

deriving via WrapFunctor ZipList instance CSemialign ZipList
deriving via WrapFunctor ZipList instance CAlign ZipList
deriving via WrapFunctor Identity instance CSemialign Identity
deriving via WrapFunctor NonEmpty instance CSemialign NonEmpty
deriving via WrapFunctor IntMap instance CSemialign IntMap
deriving via WrapFunctor IntMap instance CAlign IntMap
deriving via WrapFunctor Tree instance CSemialign Tree
deriving via WrapFunctor Seq instance CSemialign Seq
deriving via WrapFunctor Seq instance CAlign Seq
deriving via WrapFunctor V.Vector instance CSemialign V.Vector
deriving via WrapFunctor V.Vector instance CAlign V.Vector
deriving via WrapFunctor Proxy instance CSemialign Proxy
deriving via WrapFunctor Proxy instance CAlign Proxy
deriving via WrapFunctor (Map k) instance Ord k => CSemialign (Map k)
deriving via WrapFunctor (Map k) instance Ord k => CAlign (Map k)
deriving via WrapFunctor (HashMap k)
  instance (Eq k, Hashable k) => CSemialign (HashMap k)
deriving via WrapFunctor (HashMap k)
  instance (Eq k, Hashable k) => CAlign (HashMap k)
deriving via WrapFunctor ((->) s) instance CSemialign ((->) s)

instance (CSemialign f, CSemialign g) => CSemialign (SOP.Product f g) where
  calign (SOP.Pair a b) (SOP.Pair c d) = SOP.Pair (calign a c) (calign b d)
  {-# INLINE [1] calign #-}
  calignWith f (SOP.Pair a b) (SOP.Pair c d) =
    SOP.Pair (calignWith f a c) (calignWith f b d)
  {-# INLINE [1] calignWith #-}

instance (CAlign f, CAlign g) => CAlign (SOP.Product f g) where
  cnil = SOP.Pair cnil cnil
  {-# INLINE [1] cnil #-}

instance (CSemialign f, CSemialign g) => CSemialign (f :*: g) where
  calign ((:*:) a b) ((:*:) c d) = (:*:) (calign a c) (calign b d)
  {-# INLINE [1] calign #-}
  calignWith f ((:*:) a b) ((:*:) c d) =
    (:*:) (calignWith f a c) (calignWith f b d)
  {-# INLINE [1] calignWith #-}

instance (CAlign f, CAlign g) => CAlign (f :*: g) where
  cnil = cnil :*: cnil
  {-# INLINE [1] cnil #-}

instance (CSemialign f, CSemialign g) => CSemialign (Compose f g) where
  calignWith f (Compose x) (Compose y) = Compose (calignWith g x y)
    where
      g (This ga)     = cmap (f . This) ga
      g (That gb)     = cmap (f . That) gb
      g (These ga gb) = calignWith f ga gb
  {-# INLINE [1] calignWith #-}

instance (CAlign f, CSemialign g) => CAlign (Compose f g) where
  cnil = Compose cnil
  {-# INLINE [1] cnil #-}

instance (CSemialign f, CSemialign g) => CSemialign ((:.:) f g) where
  calignWith f (Comp1 x) (Comp1 y) = Comp1 (calignWith g x y)
    where
      g (This ga)     = cmap (f . This) ga
      g (That gb)     = cmap (f . That) gb
      g (These ga gb) = calignWith f ga gb
  {-# INLINE [1] calignWith #-}

instance (CAlign f, CSemialign g) => CAlign ((:.:) f g) where
  cnil = Comp1 cnil
  {-# INLINE [1] cnil #-}

instance CSemialign U.Vector where
  calignWith = alignVectorWith
  {-# INLINE [1] calignWith #-}

instance CAlign U.Vector where
  cnil = U.empty
  {-# INLINE [1] cnil #-}

instance CSemialign S.Vector where
  calignWith = alignVectorWith
  {-# INLINE [1] calignWith #-}

instance CAlign S.Vector where
  cnil = S.empty
  {-# INLINE [1] cnil #-}

instance CSemialign P.Vector where
  calignWith = alignVectorWith
  {-# INLINE [1] calignWith #-}

instance CAlign P.Vector where
  cnil = P.empty
  {-# INLINE [1] cnil #-}
