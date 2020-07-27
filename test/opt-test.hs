{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -dsuppress-idinfo -dsuppress-coercions
      -dsuppress-type-applications
      -dsuppress-module-prefixes -dsuppress-type-signatures
      -dsuppress-uniques #-}
module Main where
import Control.Subcategory.Foldable
import Control.Subcategory.Functor

import           Data.IntSet         (IntSet)
import qualified Data.IntSet         as IS
import qualified Data.Sequence       as Seq
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as U
import           Shared
import           Test.Hspec
import           Test.Inspection

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

cfoldr_uvec :: (Bool -> Integer -> Integer) -> Integer -> U.Vector Bool -> Integer
cfoldr_uvec = cfoldr

foldr_uvec :: (Bool -> Integer -> Integer) -> Integer -> U.Vector Bool -> Integer
foldr_uvec = U.foldr

main :: IO ()
main = hspec $ do
  describe "cmap" $ do
    describe "list" $
      $(inspecting "has the same representation as Prelude.map"
        $ 'cmap_list ==- 'map_list
      )
    describe "Seq" $
      $(inspecting "has the same representation as fmap"
        $ 'cmap_seq ==- 'map_seq
      )
    describe "IntSet" $
      $(inspecting "has the same representation as IntSet.map"
        $ 'cmap_intset ==- 'map_intset
      )
    describe "BVector" $
      $(inspecting "has the same representation as V.map"
        $ 'cmap_bvec ==- 'map_bvec
      )
    describe "UVector" $
      $(inspecting "has the same representation as U.map"
        $ 'cmap_uvec ==- 'map_uvec
      )
  describe "cfoldrmap" $ do
    describe "UVector" $
      $(inspecting "has the same representation as U.foldr"
        $ 'cfoldr_uvec ==- 'foldr_uvec
      )
