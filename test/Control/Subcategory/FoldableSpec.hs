{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -dsuppress-idinfo -dsuppress-coercions
      -dsuppress-type-applications
      -dsuppress-module-prefixes -dsuppress-type-signatures
      -dsuppress-uniques #-}
module Control.Subcategory.FoldableSpec where
import Control.Subcategory.Foldable

import qualified Data.Vector.Unboxed as U
import           Shared
import           Test.Inspection
import           Test.Tasty

cfoldr_uvec :: (Bool -> Integer -> Integer) -> Integer -> U.Vector Bool -> Integer
cfoldr_uvec = cfoldr

foldr_uvec :: (Bool -> Integer -> Integer) -> Integer -> U.Vector Bool -> Integer
foldr_uvec = U.foldr

test_cfoldr :: TestTree
test_cfoldr = testGroup "cfoldr"
  [testGroup "UVector"
    [ $(inspecting "has the same representation as U.foldr"
        $ 'cfoldr_uvec ==- 'foldr_uvec
      )
    ]
  ]
