{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -dsuppress-idinfo -dsuppress-coercions
      -dsuppress-type-applications
      -dsuppress-module-prefixes -dsuppress-type-signatures
      -dsuppress-uniques #-}
module Control.Subcategory.FunctorSpec where
import Control.Subcategory.Functor

import qualified Data.ByteString           as BS
import           Data.Hashable             (Hashable)
import           Data.HashSet              (HashSet)
import qualified Data.HashSet              as HS
import           Data.IntSet               (IntSet)
import qualified Data.IntSet               as IS
import qualified Data.Primitive.Array      as A
import qualified Data.Primitive.PrimArray  as PA
import qualified Data.Primitive.SmallArray as SA
import qualified Data.Sequence             as Seq
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import qualified Data.Text                 as T
import qualified Data.Vector               as V
import qualified Data.Vector.Primitive     as P
import qualified Data.Vector.Storable      as S
import qualified Data.Vector.Unboxed       as U
import           Data.Word                 (Word8)
import           Shared
import           Test.Inspection
import           Test.Tasty

cmap_list :: (a -> b) -> [a] -> [b]
cmap_list = cmap

map_list :: (a -> b) -> [a] -> [b]
map_list = map

cmap_seq :: (a -> b) -> Seq.Seq a -> Seq.Seq b
cmap_seq = cmap

map_seq :: (a -> b) -> Seq.Seq a -> Seq.Seq b
map_seq = fmap

cmap_intset :: (Int -> Int) -> WrapMono IntSet Int -> WrapMono IntSet Int
cmap_intset = cmap

map_intset :: (Int -> Int) -> IntSet -> IntSet
map_intset = IS.map

cmap_uvec :: (Int -> Bool) -> U.Vector Int -> U.Vector Bool
cmap_uvec = cmap

map_uvec :: (Int -> Bool) -> U.Vector Int -> U.Vector Bool
map_uvec = U.map

cmap_bvec :: (a -> b) -> V.Vector a -> V.Vector b
cmap_bvec = cmap

map_bvec :: (a -> b) -> V.Vector a -> V.Vector b
map_bvec = V.map

cmap_svec :: (Int -> Bool) -> S.Vector Int -> S.Vector Bool
cmap_svec = cmap

map_svec :: (Int -> Bool) -> S.Vector Int -> S.Vector Bool
map_svec = S.map

cmap_pvec :: (Int -> Word) -> P.Vector Int -> P.Vector Word
cmap_pvec = cmap

map_pvec :: (Int -> Word) -> P.Vector Int -> P.Vector Word
map_pvec = P.map

cmap_smallarray :: (Int -> Word) -> SA.SmallArray Int -> SA.SmallArray Word
cmap_smallarray = cmap

map_smallarray :: (Int -> Word) -> SA.SmallArray Int -> SA.SmallArray Word
map_smallarray = fmap

cmap_array :: (Int -> Word) -> A.Array Int -> A.Array Word
cmap_array = cmap

map_array :: (Int -> Word) -> A.Array Int -> A.Array Word
map_array = fmap

cmap_primarray :: (Int -> Word) -> PA.PrimArray Int -> PA.PrimArray Word
cmap_primarray = cmap

map_primarray :: (Int -> Word) -> PA.PrimArray Int -> PA.PrimArray Word
map_primarray = PA.mapPrimArray

cmap_Maybe :: (a -> b) -> Maybe a -> Maybe b
cmap_Maybe = cmap

map_Maybe :: (a -> b) -> Maybe a -> Maybe b
map_Maybe = fmap

cmap_Set :: (Ord b) => (Int -> b) -> Set Int -> Set b
cmap_Set = cmap

map_Set :: Ord b => (Int -> b) -> Set Int -> Set b
map_Set = Set.map

map_Set_eta :: Ord b => (Int -> b) -> Set Int -> Set b
map_Set_eta a b = Set.map a b

cmap_HashSet
  :: (Hashable b, Eq b)
  => (String -> Maybe b) -> HashSet String -> HashSet (Maybe b)
cmap_HashSet = cmap

map_HashSet
  :: (Hashable b, Eq b) => (String -> Maybe b) -> HashSet String -> HashSet (Maybe b)
{-# INLINE map_HashSet #-}
map_HashSet = HS.map

cmap_MonoBS :: (Word8 -> Word8) -> WrapMono BS.ByteString Word8 -> WrapMono BS.ByteString Word8
cmap_MonoBS = cmap

map_BS :: (Word8 -> Word8) -> BS.ByteString -> BS.ByteString
{-# INLINE map_BS #-}
map_BS = BS.map

cmap_MonoText :: (Char -> Char) -> WrapMono T.Text Char -> WrapMono T.Text Char
cmap_MonoText = cmap

map_Text :: (Char -> Char) -> T.Text -> T.Text
{-# INLINE map_Text #-}
map_Text = T.map

test_cmap :: TestTree
test_cmap = testGroup "cmap"
  [ testGroup "list"
    [ $(inspecting "has the same representation as Prelude.map"
        $ 'cmap_list ==- 'map_list
      )
    ]
  , testGroup "Seq"
    [ $(inspecting "has the same representation as fmap"
        $ 'cmap_seq ==- 'map_seq
      )
    , $(inspecting "has no instance dictionary"
      $ hasNoTypeClasses 'cmap_seq
      )
    ]
  , testGroup "IntSet"
    [ $(inspecting "has the same representation as IntSet.map"
        $ 'cmap_intset ==- 'map_intset
      )
    ]
  , testGroup "BVector"
    [ $(inspecting "has the same representation as V.map"
        $ 'cmap_bvec ==- 'map_bvec
      )
    ]
  , testGroup "UVector"
    [ $(inspecting "has the same representation as U.map"
        $ 'cmap_uvec ==- 'map_uvec
      )
    ]
  , testGroup "SVector"
    [ $(inspecting "has the same representation as S.map"
        $ 'cmap_svec ==- 'map_svec
      )
    ]
  , testGroup "PVector"
    [ $(inspecting "has the same representation as P.map"
        $ 'cmap_pvec ==- 'map_pvec
      )
    ]
  , testGroup "SmallArray"
    [ $(inspecting "has the same representation as fmap"
        $ 'cmap_smallarray ==- 'map_smallarray
      )
    , $(inspecting "has no instance dictionary"
      $ hasNoTypeClasses 'cmap_smallarray
      )
    ]
  , testGroup "Array"
    [ $(inspecting "has the same representation as fmap"
        $ 'cmap_array ==- 'map_array
      )
    , $(inspecting "has no instance dictionary"
      $ hasNoTypeClasses 'cmap_array
      )
    ]
  , testGroup "PrimArray"
    [ $(inspecting "has the same representation as PA.mapPrimArray"
        $ 'cmap_primarray ==- 'map_primarray
      )
    , $(inspecting "has no instance dictionary"
      $ hasNoTypeClasses 'cmap_primarray
      )
    ]
  , testGroup "Maybe"
    [ $(inspecting "has the same representation as fmap"
      $ 'cmap_Maybe ==- 'map_Maybe
      )
    , $(inspecting "has no instance dictionary"
      $ hasNoTypeClasses 'cmap_Maybe
      )
    ]
  , testGroup "Set"
    [ $(inspecting "has the same representation as Set.map"
      $ 'cmap_Set ==- if ghcVer >= GHC9_0 then 'map_Set_eta else 'map_Set
      )
    , $(inspecting "has no instance dictionary except Ord"
      $ 'cmap_Set `hasNoTypeClassesExcept` [''Ord]
      )
    ]
  , testGroup "HashSet"
    [ $(inspecting "has the same representation as HS.map, if the first argument is concrete"
      $ 'cmap_HashSet ==- 'map_HashSet
      )
    , $(inspecting "has no instance dictionary except EQ and Hashable"
      $ 'cmap_HashSet `hasNoTypeClassesExcept` [''Eq, ''Hashable]
      )
    ]
  , testGroup "WrapMono ByteString"
    [ $(inspecting "has the same representation as Data.ByteString.map"
      $ 'cmap_MonoBS ==- 'map_BS
      )
    , $(inspecting "has no instance dictionary"
      $ hasNoTypeClasses 'cmap_MonoBS
      )
    ]
  , testGroup "WrapMono Text"
    [ $(inspecting "has the same representation as Data.Text.map"
      $ 'cmap_MonoText ==- 'map_Text
      )
    , $(inspecting "has no instance dictionary"
      $ hasNoTypeClasses 'cmap_MonoText
      )
    ]
  ]
