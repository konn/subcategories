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

emap_list :: (a -> b) -> [a] -> [b]
emap_list = emap

map_list :: (a -> b) -> [a] -> [b]
map_list = map

emap_seq :: (a -> b) -> Seq.Seq a -> Seq.Seq b
emap_seq = emap

map_seq :: (a -> b) -> Seq.Seq a -> Seq.Seq b
map_seq = fmap

emap_intset :: (Int -> Int) -> WrapMono IntSet Int -> WrapMono IntSet Int
emap_intset = emap

map_intset :: (Int -> Int) -> IntSet -> IntSet
map_intset = IS.map

emap_uvec :: (Int -> Bool) -> U.Vector Int -> U.Vector Bool
emap_uvec = emap

map_uvec :: (Int -> Bool) -> U.Vector Int -> U.Vector Bool
map_uvec = U.map

emap_bvec :: (a -> b) -> V.Vector a -> V.Vector b
emap_bvec = emap

map_bvec :: (a -> b) -> V.Vector a -> V.Vector b
map_bvec = V.map

cfoldr_uvec :: (Bool -> Integer -> Integer) -> Integer -> U.Vector Bool -> Integer
cfoldr_uvec = cfoldr

foldr_uvec :: (Bool -> Integer -> Integer) -> Integer -> U.Vector Bool -> Integer
foldr_uvec = U.foldr

main :: IO ()
main = hspec $ do
  describe "emap" $ do
    describe "list" $
      $(inspecting "has the same representation as Prelude.map"
        $ 'emap_list ==- 'map_list
      )
    describe "Seq" $
      $(inspecting "has the same representation as fmap"
        $ 'emap_seq ==- 'map_seq
      )
    describe "IntSet" $
      $(inspecting "has the same representation as IntSet.map"
        $ 'emap_intset ==- 'map_intset
      )
    describe "BVector" $
      $(inspecting "has the same representation as V.map"
        $ 'emap_bvec ==- 'map_bvec
      )
    describe "UVector" $
      $(inspecting "has the same representation as U.map"
        $ 'emap_uvec ==- 'map_uvec
      )
  describe "cfoldrmap" $ do
    describe "UVector" $
      $(inspecting "has the same representation as U.foldr"
        $ 'cfoldr_uvec ==- 'foldr_uvec
      )
