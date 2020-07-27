{-# LANGUAGE CPP, DefaultSignatures, DerivingVia, LambdaCase            #-}
{-# LANGUAGE QuantifiedConstraints, StandaloneDeriving, TemplateHaskell #-}
{-# LANGUAGE TypeOperators                                              #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Control.Subcategory.Foldable
  ( CFoldable(..), CTraversable(..),
    CFreeMonoid(), cctraverseFreeMonoid
  ) where
import           Control.Applicative                  (ZipList, getZipList)
import           Control.Monad
import           Control.Subcategory.Applicative
import           Control.Subcategory.Functor
import           Control.Subcategory.Pointed
import           Control.Subcategory.Wrapper.Internal
import           Data.Coerce
import           Data.Complex                         (Complex)
import           Data.Foldable
import           Data.Functor.Const                   (Const)
import           Data.Functor.Identity                (Identity)
import qualified Data.Functor.Product                 as SOP
import qualified Data.Functor.Sum                     as SOP
import qualified Data.HashMap.Strict                  as HM
import qualified Data.HashSet                         as HS
import qualified Data.IntMap.Strict                   as IM
import qualified Data.IntSet                          as IS
import           Data.Kind                            (Type)
import           Data.List.NonEmpty                   (NonEmpty)
import qualified Data.List.NonEmpty                   as NE
import qualified Data.Map                             as M
import           Data.Maybe                           (fromMaybe)
import           Data.Monoid
import qualified Data.Monoid                          as Mon
import           Data.MonoTraversable                 hiding (WrappedMono,
                                                       unwrapMono)
import           Data.Ord                             (Down)
import           Data.Proxy                           (Proxy)
import           Data.Semigroup                       (Arg, Max (..), Min (..),
                                                       Option)
import qualified Data.Semigroup                       as Sem
import qualified Data.Sequence                        as Seq
import           Data.Sequences                       (IsSequence (indexEx))
import qualified Data.Sequences                       as MT
import qualified Data.Set                             as Set
import qualified Data.Vector                          as V
import qualified Data.Vector.Generic                  as G
import qualified Data.Vector.Primitive                as P
import qualified Data.Vector.Storable                 as S
import qualified Data.Vector.Unboxed                  as U
import           Foreign.Ptr                          (Ptr)
import           GHC.Generics
import           Language.Haskell.TH                  hiding (Type)

-- See Note [Function coercion]
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce
{-# INLINE (#.) #-}

class Constrained f => CFoldable f where
  {-# MINIMAL cfoldMap | cfoldr #-}
  cfoldMap :: (Cat f a, Monoid w) => (a -> w) -> f a -> w
  {-# INLINE [1] cfoldMap #-}
  cfoldMap f = cfoldr (mappend . f) mempty

  cfoldMap' :: (Cat f a, Monoid m) => (a -> m) -> f a -> m
  {-# INLINE [1] cfoldMap' #-}
  cfoldMap' f = cfoldl' (\ acc a -> acc <> f a) mempty

  cfold :: (Cat f w, Monoid w) => f w -> w
  cfold = cfoldMap id

  {-# INLINE [1] cfold #-}
  cfoldr :: (Cat f a) => (a -> b -> b) -> b -> f a -> b
  {-# INLINE [1] cfoldr #-}
  cfoldr f z t = appEndo (cfoldMap (Endo #. f) t) z

  cfoldl
      :: (Cat f a)
      => (b -> a -> b) -> b -> f a -> b
  {-# INLINE [1] cfoldl #-}
  cfoldl f z t = appEndo (getDual (cfoldMap (Dual . Endo . flip f) t)) z

  cfoldr' :: (Cat f a) => (a -> b -> b) -> b -> f a -> b
  {-# INLINE [1] cfoldr' #-}
  cfoldr' f z0 xs = cfoldl f' id xs z0
      where f' k x z = k $! f x z

  cfoldl' :: Cat f a => (b -> a -> b) -> b -> f a -> b
  {-# INLINE [1] cfoldl' #-}
  cfoldl' f z0 xs = cfoldr f' id xs z0
    where f' x k z = k $! f z x

  ctoList :: Cat f a => f a -> [a]
  {-# INLINE [1] ctoList #-}
  ctoList = cfoldr (:) []

  cfoldr1 :: Cat f a => (a -> a -> a) -> f a -> a
  {-# INLINE [1] cfoldr1 #-}
  cfoldr1 f xs = fromMaybe (errorWithoutStackTrace "cfoldr1: empty structure")
                    (cfoldr mf Nothing xs)
      where
        mf x m = Just $
          case m of
            Nothing -> x
            Just y  -> f x y

  cfoldl1 :: Cat f a => (a -> a -> a) -> f a -> a
  {-# INLINE [1] cfoldl1 #-}
  cfoldl1 f xs = fromMaybe (errorWithoutStackTrace "cfoldl1: empty structure")
                  (cfoldl mf Nothing xs)
    where
      mf m y = Just $
        case m of
          Nothing -> y
          Just x  -> f x y

  cindex :: Cat f a => f a -> Int -> a
  cindex xs n = case cfoldl' go (Left' 0) xs of
    Right' x -> x
    Left'{} -> errorWithoutStackTrace $ "cindex: index out of bound " ++ show n
    where
      go (Left' i) x
        | i == n = Right' x
        | otherwise = Left' (i + 1)
      go r@Right'{} _ = r

  cnull :: Cat f a => f a -> Bool
  cnull = cfoldr (const $ const False) True

  clength :: Cat f a => f a -> Int
  {-# INLINE [1] clength #-}
  clength = cfoldl' (\c _ -> c + 1) 0

  cany :: Cat f a => (a -> Bool) -> f a -> Bool
  {-# INLINE [1] cany #-}
  cany p = cfoldl' (\b -> (||) b . p) False

  call :: Cat f a => (a -> Bool) -> f a -> Bool
  {-# INLINE [1] call #-}
  call p = cfoldl' (\b -> (&&) b . p) True

  celem :: (Eq a, Cat f a) => a -> f a -> Bool
  {-# INLINE [1] celem #-}
  celem = cany . (==)

  cminimum :: (Ord a, Cat f a) => f a -> a
  {-# INLINE [1] cminimum #-}
  cminimum =
    getMin
    . fromMaybe (errorWithoutStackTrace "minimum: empty structure")
    . cfoldMap (Just . Min)

  cmaximum :: (Ord a, Cat f a) => f a -> a
  {-# INLINE [1] cmaximum #-}
  cmaximum =
    getMax
    . fromMaybe (errorWithoutStackTrace "cmaximum: empty structure")
    . cfoldMap (Just . Max)

  csum :: (Num a, Cat f a) => f a -> a
  {-# INLINE [1] csum #-}
  csum = getSum #. cfoldMap Sum

  cproduct :: (Num a, Cat f a) => f a -> a
  {-# INLINE [1] cproduct #-}
  cproduct = getProduct #. cfoldMap Product

  cctraverse_
    :: (CApplicative g, CPointed g, Cat g (), Cat f a, Cat g b)
    => (a -> g b)
    -> f a -> g ()
  {-# INLINE [1] cctraverse_ #-}
  cctraverse_ f = cfoldr c (cpure ())
    where
      {-# INLINE c #-}
      c x k = f x .> k

  ctraverse_
    :: (Applicative g, CPointed g, Cat g (), Cat f a, Cat g b)
    => (a -> g b)
    -> f a -> g ()
  {-# INLINE [1] ctraverse_ #-}
  ctraverse_ f = cfoldr c (cpure ())
    where
      {-# INLINE c #-}
      c x k = f x *> k

data Eith' a b = Left' !a | Right' !b

instance Traversable f => CTraversable (WrapFunctor f) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}

instance Foldable f => CFoldable (WrapFunctor f) where
  cfoldMap = foldMap
  {-# INLINE [1] cfoldMap #-}
#if MIN_VERSION_base(4,13,0)
  cfoldMap' = foldMap'
  {-# INLINE [1] cfoldMap' #-}
#endif
  cfold = fold
  {-# INLINE [1] cfold #-}
  cfoldr = foldr
  {-# INLINE [1] cfoldr #-}
  cfoldr' = foldr'
  {-# INLINE [1] cfoldr' #-}
  cfoldl = foldl
  {-# INLINE [1] cfoldl #-}
  cfoldl' = foldl'
  {-# INLINE [1] cfoldl' #-}
  ctoList = toList
  {-# INLINE [1] ctoList #-}
  cfoldr1 = foldr1
  {-# INLINE [1] cfoldr1 #-}
  cfoldl1 = foldl1
  {-# INLINE [1] cfoldl1 #-}
  cnull = null
  {-# INLINE [1] cnull #-}
  clength = length
  {-# INLINE [1] clength #-}
  cany = any
  {-# INLINE [1] cany #-}
  call = all
  {-# INLINE [1] call #-}
  celem = elem
  {-# INLINE [1] celem #-}
  cminimum = minimum
  {-# INLINE [1] cminimum #-}
  cmaximum = maximum
  {-# INLINE [1] cmaximum #-}
  csum = sum
  {-# INLINE [1] csum #-}
  cproduct = product
  {-# INLINE [1] cproduct #-}
  ctraverse_ = traverse_
  {-# INLINE [1] ctraverse_ #-}

{-# RULES
"cctraverse_/traverse_"
  forall (f :: Applicative f => a -> f b) (tx :: Foldable t => t a).
  cctraverse_ f tx = traverse_ f tx
  #-}

{-# RULES
"cindex/List"
  cindex = (!!)
  #-}

class (CFunctor f, CFoldable f) => CTraversable f where
  -- | __N.B.__ If we require @g@ to be 'CApplicative'
  --   we cannot directly lift plain 'Traversable' to 'CTraversable'.
  --   This is rather annoying, so we require the strongest possible
  --   constraint to @g@ here.
  ctraverse
    :: (Cat f a, Cat f b, Applicative g)
    => (a -> g b) -> f a -> g (f b)

deriving via WrapFunctor []
  instance CFoldable []
instance CTraversable [] where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor Maybe
  instance CFoldable Maybe
instance CTraversable Maybe where
  ctraverse = traverse
deriving via WrapFunctor (Either e)
  instance CFoldable (Either e)
instance CTraversable (Either e) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor IM.IntMap
  instance CFoldable IM.IntMap
instance CTraversable IM.IntMap where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor (M.Map k)
  instance CFoldable (M.Map k)
instance Ord k => CTraversable (M.Map k) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor (HM.HashMap k)
  instance CFoldable (HM.HashMap k)
instance CTraversable (HM.HashMap k) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor Seq.Seq
  instance CFoldable Seq.Seq
instance CTraversable Seq.Seq where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
{-# RULES
"cindex/Seq"
  cindex = Seq.index
  #-}

deriving via WrapFunctor Par1
  instance CFoldable Par1
instance CTraversable Par1 where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor NonEmpty
  instance CFoldable NonEmpty
instance CTraversable NonEmpty where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
{-# RULES
"cindex/NonEmpty"
  cindex = (NE.!!)
  #-}

deriving via WrapFunctor Down
  instance CFoldable Down
instance CTraversable Down where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor Mon.Last
  instance CFoldable Mon.Last
instance CTraversable Mon.Last where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor Mon.First
  instance CFoldable Mon.First
instance CTraversable Mon.First where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor Sem.Last
  instance CFoldable Sem.Last
instance CTraversable Sem.Last where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor Sem.First
  instance CFoldable Sem.First
instance CTraversable Sem.First where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor Identity
  instance CFoldable Identity
instance CTraversable Identity where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor ZipList
  instance CFoldable ZipList
instance CTraversable ZipList where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
{-# RULES
"cindex/ZipList"
  cindex = (!!) . getZipList
  #-}

deriving via WrapFunctor Option
  instance CFoldable Option
instance CTraversable Option where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor Min
  instance CFoldable Min
instance CTraversable Min where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor Max
  instance CFoldable Max
instance CTraversable Max where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor Complex
  instance CFoldable Complex
instance CTraversable Complex where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor (V1 :: Type -> Type)
  instance CFoldable (V1 :: Type -> Type)
instance CTraversable (V1 :: Type -> Type) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor (U1 :: Type -> Type)
  instance CFoldable (U1 :: Type -> Type)
instance CTraversable (U1 :: Type -> Type) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor ((,) a)
  instance CFoldable ((,) a)
instance CTraversable ((,) a) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor (Proxy :: Type -> Type)
  instance CFoldable (Proxy :: Type -> Type)
instance CTraversable (Proxy :: Type -> Type) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor (Arg a)
  instance CFoldable (Arg a)
instance CTraversable (Arg a) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor (Rec1 (f :: Type -> Type))
  instance Foldable f => CFoldable (Rec1 (f :: Type -> Type))
deriving via WrapFunctor (URec Char :: Type -> Type)
  instance CFoldable (URec Char :: Type -> Type)
instance CTraversable (URec Char :: Type -> Type) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor (URec Double :: Type -> Type)
  instance CFoldable (URec Double :: Type -> Type)
instance CTraversable (URec Double :: Type -> Type) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor (URec Float :: Type -> Type)
  instance CFoldable (URec Float :: Type -> Type)
instance CTraversable (URec Float :: Type -> Type) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor (URec Int :: Type -> Type)
  instance CFoldable (URec Int :: Type -> Type)
instance CTraversable (URec Int :: Type -> Type) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor (URec Word :: Type -> Type)
  instance CFoldable (URec Word :: Type -> Type)
instance CTraversable (URec Word :: Type -> Type) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor (URec (Ptr ()) :: Type -> Type)
  instance CFoldable (URec (Ptr ()) :: Type -> Type)
instance CTraversable (URec (Ptr ()) :: Type -> Type) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving newtype
  instance CFoldable f => CFoldable (Alt f)
deriving newtype
  instance CFoldable f => CFoldable (Ap f)
deriving via WrapFunctor (Const m :: Type -> Type)
  instance CFoldable (Const m :: Type -> Type)
instance CTraversable (Const m :: Type -> Type) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor (K1 i c :: Type -> Type)
  instance CFoldable (K1 i c :: Type -> Type)
instance CTraversable (K1 i c :: Type -> Type) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}

instance (CFoldable f, CFoldable g) => CFoldable (f :+: g) where
  {-# INLINE [1] cfoldMap #-}
  cfoldMap f = \case
    L1 x -> cfoldMap f x
    R1 x -> cfoldMap f x

  {-# INLINE [1] cfoldr #-}
  cfoldr f z = \case
    L1 x -> cfoldr f z x
    R1 x -> cfoldr f z x

  cfoldMap' = \f -> \case
    L1 x -> cfoldMap' f x
    R1 x -> cfoldMap' f x
  {-# INLINE [1] cfoldMap' #-}
  cfold = \case
    L1 x -> cfold x
    R1 x -> cfold x
  {-# INLINE [1] cfold #-}
  cfoldr' = \f z -> \case
    L1 x -> cfoldr' f z x
    R1 x -> cfoldr' f z x
  {-# INLINE [1] cfoldr' #-}
  cfoldl = \f z -> \case
    L1 x -> cfoldl f z x
    R1 x -> cfoldl f z x
  {-# INLINE [1] cfoldl #-}
  cfoldl' = \f z -> \case
    L1 x -> cfoldl' f z x
    R1 x -> cfoldl' f z x
  {-# INLINE [1] cfoldl' #-}
  ctoList = \case
    L1 x -> ctoList x
    R1 x -> ctoList x
  {-# INLINE [1] ctoList #-}
  cfoldr1 = \f -> \case
    L1 x -> cfoldr1 f x
    R1 x -> cfoldr1 f x
  {-# INLINE [1] cfoldr1 #-}
  cfoldl1 = \f -> \case
    L1 x -> cfoldl1 f x
    R1 x -> cfoldl1 f x
  {-# INLINE [1] cfoldl1 #-}
  cnull = \case
    L1 x -> cnull x
    R1 x -> cnull x
  {-# INLINE [1] cnull #-}
  clength = \case
    L1 x -> clength x
    R1 x -> clength x
  {-# INLINE [1] clength #-}
  cany = \f -> \case
    L1 x -> cany f x
    R1 x -> cany f x
  {-# INLINE [1] cany #-}
  call = \f -> \case
    L1 x -> call f x
    R1 x -> call f x
  {-# INLINE [1] call #-}
  celem = \x -> \case
    L1 xs -> celem x xs
    R1 xs -> celem x xs
  {-# INLINE [1] celem #-}
  cminimum = \case
    L1 xs -> cminimum xs
    R1 xs -> cminimum xs
  {-# INLINE [1] cminimum #-}
  cmaximum = \case
    L1 xs -> cmaximum xs
    R1 xs -> cmaximum xs
  {-# INLINE [1] cmaximum #-}
  csum = \case
    L1 xs -> csum xs
    R1 xs -> csum xs
  {-# INLINE [1] csum #-}
  cproduct = \case
    L1 xs -> cproduct xs
    R1 xs -> cproduct xs
  {-# INLINE [1] cproduct #-}
  ctraverse_ f = \case
    L1 xs -> ctraverse_ f xs
    R1 xs -> ctraverse_ f xs
  {-# INLINE [1] ctraverse_ #-}

instance (CTraversable f, CTraversable g) => CTraversable (f :+: g) where
  ctraverse f = \case
    L1 xs -> L1 <$> ctraverse f xs
    R1 xs -> R1 <$> ctraverse f xs
  {-# INLINE [1] ctraverse #-}

instance (CFoldable f, CFoldable g) => CFoldable (f :*: g) where
  {-# INLINE [1] cfoldMap #-}
  cfoldMap f (l :*: r) = cfoldMap f l <> cfoldMap f r

  cfoldMap' f (l :*: r) = cfoldMap' f l <> cfoldMap' f r
  {-# INLINE [1] cfoldMap' #-}
  cfold (l :*: r) = cfold l <> cfold r
  {-# INLINE [1] cfold #-}
  cnull (l :*: r) = cnull l && cnull r
  {-# INLINE [1] cnull #-}
  clength (l :*: r) = clength l + clength r
  {-# INLINE [1] clength #-}
  cany f (l :*: r) = cany f l || cany f r
  {-# INLINE [1] cany #-}
  call f (l :*: r) = call f l && call f r
  {-# INLINE [1] call #-}
  celem x (l :*: r) = celem x l || celem x r
  {-# INLINE [1] celem #-}
  csum (l :*: r) = csum l + csum r
  {-# INLINE [1] csum #-}
  cproduct (l :*: r) = cproduct l * cproduct r
  {-# INLINE [1] cproduct #-}
  ctraverse_ f (l :*: r) = ctraverse_ f l *> ctraverse_ f r
  {-# INLINE [1] ctraverse_ #-}

instance (CTraversable f, CTraversable g) => CTraversable (f :*: g) where
  ctraverse f (l :*: r) =
    (:*:) <$> ctraverse f l <*> ctraverse f r

instance (CFoldable f, CFoldable g) => CFoldable (SOP.Sum f g) where
  {-# INLINE [1] cfoldMap #-}
  cfoldMap f = \case
    SOP.InL x -> cfoldMap f x
    SOP.InR x -> cfoldMap f x

  {-# INLINE [1] cfoldr #-}
  cfoldr f z = \case
    SOP.InL x -> cfoldr f z x
    SOP.InR x -> cfoldr f z x

  cfoldMap' = \f -> \case
    SOP.InL x -> cfoldMap' f x
    SOP.InR x -> cfoldMap' f x
  {-# INLINE [1] cfoldMap' #-}
  cfold = \case
    SOP.InL x -> cfold x
    SOP.InR x -> cfold x
  {-# INLINE [1] cfold #-}
  cfoldr' = \f z -> \case
    SOP.InL x -> cfoldr' f z x
    SOP.InR x -> cfoldr' f z x
  {-# INLINE [1] cfoldr' #-}
  cfoldl = \f z -> \case
    SOP.InL x -> cfoldl f z x
    SOP.InR x -> cfoldl f z x
  {-# INLINE [1] cfoldl #-}
  cfoldl' = \f z -> \case
    SOP.InL x -> cfoldl' f z x
    SOP.InR x -> cfoldl' f z x
  {-# INLINE [1] cfoldl' #-}
  ctoList = \case
    SOP.InL x -> ctoList x
    SOP.InR x -> ctoList x
  {-# INLINE [1] ctoList #-}
  cfoldr1 = \f -> \case
    SOP.InL x -> cfoldr1 f x
    SOP.InR x -> cfoldr1 f x
  {-# INLINE [1] cfoldr1 #-}
  cfoldl1 = \f -> \case
    SOP.InL x -> cfoldl1 f x
    SOP.InR x -> cfoldl1 f x
  {-# INLINE [1] cfoldl1 #-}
  cnull = \case
    SOP.InL x -> cnull x
    SOP.InR x -> cnull x
  {-# INLINE [1] cnull #-}
  clength = \case
    SOP.InL x -> clength x
    SOP.InR x -> clength x
  {-# INLINE [1] clength #-}
  cany = \f -> \case
    SOP.InL x -> cany f x
    SOP.InR x -> cany f x
  {-# INLINE [1] cany #-}
  call = \f -> \case
    SOP.InL x -> call f x
    SOP.InR x -> call f x
  {-# INLINE [1] call #-}
  celem = \x -> \case
    SOP.InL xs -> celem x xs
    SOP.InR xs -> celem x xs
  {-# INLINE [1] celem #-}
  cminimum = \case
    SOP.InL xs -> cminimum xs
    SOP.InR xs -> cminimum xs
  {-# INLINE [1] cminimum #-}
  cmaximum = \case
    SOP.InL xs -> cmaximum xs
    SOP.InR xs -> cmaximum xs
  {-# INLINE [1] cmaximum #-}
  csum = \case
    SOP.InL xs -> csum xs
    SOP.InR xs -> csum xs
  {-# INLINE [1] csum #-}
  cproduct = \case
    SOP.InL xs -> cproduct xs
    SOP.InR xs -> cproduct xs
  {-# INLINE [1] cproduct #-}
  ctraverse_ f = \case
    SOP.InL xs -> ctraverse_ f xs
    SOP.InR xs -> ctraverse_ f xs
  {-# INLINE [1] ctraverse_ #-}

instance (CTraversable f, CTraversable g) => CTraversable (SOP.Sum f g) where
  ctraverse f = \case
    SOP.InL xs -> SOP.InL <$> ctraverse f xs
    SOP.InR xs -> SOP.InR <$> ctraverse f xs
  {-# INLINE [1] ctraverse #-}

instance (CFoldable f, CFoldable g) => CFoldable (SOP.Product f g) where
  {-# INLINE [1] cfoldMap #-}
  cfoldMap f (SOP.Pair l r) = cfoldMap f l <> cfoldMap f r

  cfoldMap' f (SOP.Pair l r) = cfoldMap' f l <> cfoldMap' f r
  {-# INLINE [1] cfoldMap' #-}
  cfold (SOP.Pair l r) = cfold l <> cfold r
  {-# INLINE [1] cfold #-}
  cnull (SOP.Pair l r) = cnull l && cnull r
  {-# INLINE [1] cnull #-}
  clength (SOP.Pair l r) = clength l + clength r
  {-# INLINE [1] clength #-}
  cany f (SOP.Pair l r) = cany f l || cany f r
  {-# INLINE [1] cany #-}
  call f (SOP.Pair l r) = call f l && call f r
  {-# INLINE [1] call #-}
  celem x (SOP.Pair l r) = celem x l || celem x r
  {-# INLINE [1] celem #-}
  csum (SOP.Pair l r) = csum l + csum r
  {-# INLINE [1] csum #-}
  cproduct (SOP.Pair l r) = cproduct l * cproduct r
  {-# INLINE [1] cproduct #-}
  ctraverse_ f (SOP.Pair l r) =
    ctraverse_ f l *> ctraverse_ f r
  {-# INLINE ctraverse_ #-}

instance (CTraversable f, CTraversable g) => CTraversable (SOP.Product f g) where
  {-# INLINE [1] ctraverse #-}
  ctraverse f (SOP.Pair l r) =
    SOP.Pair <$> ctraverse f l <*> ctraverse f r

instance CFoldable Set.Set where
  cfoldMap = ofoldMap
  {-# INLINE [1] cfoldMap #-}
  cfoldr = Set.foldr
  {-# INLINE [1] cfoldr #-}
  cfoldl = Set.foldl
  {-# INLINE [1] cfoldl #-}
  cfoldr' = Set.foldr'
  {-# INLINE [1] cfoldr' #-}
  cfoldl' = Set.foldl'
  {-# INLINE [1] cfoldl' #-}
  cminimum = Set.findMin
  {-# INLINE [1] cminimum #-}
  cmaximum = Set.findMax
  {-# INLINE [1] cmaximum #-}
  celem = Set.member
  {-# INLINE [1] celem #-}
  ctoList = Set.toList
  {-# INLINE [1] ctoList #-}

instance CTraversable Set.Set where
  -- TODO: more efficient implementation
  ctraverse f =
      fmap Set.fromList
    . traverse f
    . Set.toList
  {-# INLINE [1] ctraverse #-}

instance CFoldable HS.HashSet where
  cfoldMap = ofoldMap
  {-# INLINE [1] cfoldMap #-}
  cfoldr = HS.foldr
  {-# INLINE [1] cfoldr #-}
  cfoldl' = HS.foldl'
  {-# INLINE [1] cfoldl' #-}
  celem = HS.member
  {-# INLINE [1] celem #-}
  ctoList = HS.toList
  {-# INLINE [1] ctoList #-}

instance CTraversable HS.HashSet where
  -- TODO: more efficient implementation
  ctraverse f =
      fmap HS.fromList
    . traverse f
    . HS.toList
  {-# INLINE [1] ctraverse #-}

{-# RULES
"celem/IntSet"
  celem = coerce
    @(Int -> IS.IntSet -> Bool)
    @(Int -> WrapMono IS.IntSet Int -> Bool)
    IS.member
"cmaximum/IntSet"
  cmaximum = coerce @_ @(WrapMono IS.IntSet Int -> Int)
    IS.findMax
"cminimum/IntSet"
  cminimum = coerce @(IS.IntSet -> Int) @(WrapMono IS.IntSet Int -> Int)
    IS.findMin
  #-}

instance MonoFoldable mono => CFoldable (WrapMono mono) where
  cfoldMap = ofoldMap
  {-# INLINE [1] cfoldMap #-}
  cfold = ofold
  {-# INLINE [1] cfold #-}
  cfoldr = ofoldr
  {-# INLINE [1] cfoldr #-}
  cfoldl' = ofoldl'
  {-# INLINE [1] cfoldl' #-}
  ctoList = otoList
  {-# INLINE [1] ctoList #-}
  cfoldr1 = ofoldr1Ex
  {-# INLINE [1] cfoldr1 #-}
  cnull = onull
  {-# INLINE [1] cnull #-}
  clength = olength
  {-# INLINE [1] clength #-}
  cany = oany
  {-# INLINE [1] cany #-}
  call = oall
  {-# INLINE [1] call #-}
  celem = oelem
  {-# INLINE [1] celem #-}
  cminimum = minimumEx
  {-# INLINE [1] cminimum #-}
  cmaximum = maximumEx
  {-# INLINE [1] cmaximum #-}
  csum = osum
  {-# INLINE [1] csum #-}
  cproduct = oproduct
  {-# INLINE [1] cproduct #-}
  ctraverse_ = otraverse_
  {-# INLINE [1] ctraverse_ #-}

instance MonoTraversable mono => CTraversable (WrapMono mono) where
  ctraverse = \f -> fmap WrapMono . otraverse f . unwrapMono

fmap concat . forM
  (map conT [''V.Vector, ''U.Vector, ''S.Vector, ''P.Vector])
  $ \v ->
  [d|
    instance CFoldable $v where
      {-# INLINE [1] cfoldr #-}
      cfoldr = G.foldr
      {-# INLINE [1] cfoldr' #-}
      cfoldr' = G.foldr'
      {-# INLINE [1] cfoldl #-}
      cfoldl = G.foldl
      {-# INLINE [1] cfoldl' #-}
      cfoldl' = G.foldl'
      {-# INLINE [1] cindex #-}
      cindex = (G.!)
      {-# INLINE [1] celem #-}
      celem = G.elem
      {-# INLINE [1] cany #-}
      cany = G.any
      {-# INLINE [1] call #-}
      call = G.all
      {-# INLINE [1] cfoldl1 #-}
      cfoldl1 = G.foldl1
      {-# INLINE [1] cfoldr1 #-}
      cfoldr1 = G.foldr1
      {-# INLINE [1] csum #-}
      csum = G.sum
      {-# INLINE [1] cproduct #-}
      cproduct = G.product
      {-# INLINE [1] cmaximum #-}
      cmaximum = G.maximum
      {-# INLINE [1] cminimum #-}
      cminimum = G.minimum
      {-# INLINE [1] ctoList #-}
      ctoList = G.toList
    |]

instance CTraversable V.Vector where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}

instance CTraversable U.Vector where
  ctraverse = \f -> fmap V.convert . traverse f . U.convert @_ @_ @V.Vector
  {-# INLINE [1] ctraverse #-}

instance CTraversable S.Vector where
  ctraverse = \f -> fmap S.convert . traverse f . U.convert @_ @_ @V.Vector
  {-# INLINE [1] ctraverse #-}

instance CTraversable P.Vector where
  ctraverse = \f -> fmap P.convert . traverse f . U.convert @_ @_ @V.Vector
  {-# INLINE [1] ctraverse #-}

{-# RULES
"cindex/IsSequence" forall (xs :: (MT.Index mono ~ Int, IsSequence mono) => WrapMono mono b).
  cindex xs = withMonoCoercible (coerce @(mono -> Int -> Element mono) indexEx xs)
  #-}

-- | Free monoid functor from fullsubcategory.
--   It must be a pointed foldable functor with the property
--   that for any 'Monoid' @w@ and @f :: a -> w@,
--   @'cfoldMap' f@ must be a monoid homomorphism and the following
--   must be hold:
--
--    @
--       'cfoldMap' f . 'cpure' == f
--    @
--
--   Hence, @'Set's@ cannot be a free monoid functor;
class (CFunctor f, forall x. Monoid (f x), CPointed f, CFoldable f)
  => CFreeMonoid f

cctraverseFreeMonoid
  ::  ( CFreeMonoid t, CApplicative f, CPointed f,
        Cat t a, Cat f (t b), Cat f b, Cat t b,
        Cat f (t b, t b)
      )
  => (a -> f b) -> t a -> f (t b)
cctraverseFreeMonoid f =
  runCApp . cfoldMap (CApp . cmap cpure . f)

