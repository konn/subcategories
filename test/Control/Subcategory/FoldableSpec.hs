{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -dsuppress-idinfo -dsuppress-coercions
      -dsuppress-type-applications
      -dsuppress-module-prefixes -dsuppress-type-signatures
      -dsuppress-uniques #-}
module Control.Subcategory.FoldableSpec where
import Control.Subcategory.Foldable

import           Data.MonoTraversable
import qualified Data.Vector           as V
import qualified Data.Vector.Generic   as G
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Storable  as S
import qualified Data.Vector.Unboxed   as U
import           Shared
import           Test.Inspection
import           Test.Tasty

cfoldr_uvec :: (Int -> b -> b) -> b -> U.Vector Int -> b
cfoldr_uvec = cfoldr

cfoldr_uvec_poly :: U.Unbox a => (a -> b -> b) -> b -> U.Vector a -> b
cfoldr_uvec_poly = cfoldr

foldr_uvec :: (Int -> b -> b) -> b -> U.Vector Int -> b
foldr_uvec = U.foldr

cfoldr_svec :: (Int -> b -> b) -> b -> S.Vector Int -> b
cfoldr_svec = cfoldr

cfoldr_svec_poly :: S.Storable a => (a -> b -> b) -> b -> S.Vector a -> b
cfoldr_svec_poly = cfoldr

foldr_svec :: (Int -> b -> b) -> b -> S.Vector Int -> b
foldr_svec = S.foldr

cfoldr_pvec :: (Int -> b -> b) -> b -> P.Vector Int -> b
cfoldr_pvec = cfoldr

cfoldr_pvec_poly :: P.Prim a => (a -> b -> b) -> b -> P.Vector a -> b
cfoldr_pvec_poly = cfoldr

foldr_pvec :: (Int -> b -> b) -> b -> P.Vector Int -> b
foldr_pvec = P.foldr

cfoldr_bvec :: (a -> b -> b) -> b -> V.Vector a -> b
cfoldr_bvec = cfoldr

foldr_bvec :: (a -> b -> b) -> b -> V.Vector a -> b
foldr_bvec = V.foldr

cfoldr_list :: (a -> b -> b) -> b -> [a] -> b
cfoldr_list = cfoldr

foldr_list :: (a -> b -> b) -> b -> [a] -> b
foldr_list = foldr

test_cfoldr :: TestTree
test_cfoldr = testGroup "cfoldr"
  [ testGroup "List"
    [ $(inspecting "has the same representation as V.foldr"
        $ 'cfoldr_list ==- 'foldr_list
      )
    , $(inspecting "has no instance dictionary"
      $ hasNoTypeClasses 'cfoldr_list
      )
    ]
  , testGroup "BVector"
    [ $(inspecting "has the same representation as V.foldr"
        $ 'cfoldr_bvec ==- 'foldr_bvec
      )
    , $(inspecting "has no instance dictionary except G.Vector"
      $ 'cfoldr_bvec `hasNoTypeClassesExcept` [''G.Vector]
      )
    ]
  , testGroup "UVector"
    [ $(inspecting "has the same representation as U.foldr (if an element is concrete)"
        $ 'cfoldr_uvec ==- 'foldr_uvec
      )
    , $(inspecting "has no instance dictionary other than Unbox (if polymorphic)"
      $ 'cfoldr_uvec_poly `hasNoTypeClassesExcept` [''U.Unbox]
      )
    ]
  , testGroup "SVector"
    [ $(inspecting "has the same representation as S.foldr (if an element is concrete)"
        $ 'cfoldr_svec ==- 'foldr_svec
      )
    , $(inspecting "has no instance dictionary other than Storable (if polymorphic)"
      $ 'cfoldr_svec_poly `hasNoTypeClassesExcept` [''S.Storable]
      )
    ]
  , testGroup "PVector"
    [ $(inspecting "has the same representation as P.foldr (if an element is concrete)"
        $ 'cfoldr_pvec ==- 'foldr_pvec
      )
    , $(inspecting "has no instance dictionary other than Storable (if polymorphic)"
      $ 'cfoldr_pvec_poly `hasNoTypeClassesExcept` [''P.Prim]
      )
    ]
  ]

cfoldMap_uvec :: Monoid w => (Int -> w) -> U.Vector Int -> w
cfoldMap_uvec = cfoldMap

ofoldMap_uvec :: Monoid w => (Int -> w) -> U.Vector Int -> w
ofoldMap_uvec = ofoldMap

test_cfoldMap :: TestTree
test_cfoldMap = testGroup "cfoldMap"
    [ testGroup "UVector"
      [ $(inspecting "has the same rep as ofoldMap (if elements concrete)"
        $ 'cfoldMap_uvec ==- 'ofoldMap_uvec
        )
      ]
    ]
