{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Shared where

import Control.Exception
import Data.Maybe (listToMaybe)
import GHC.Stack (callStack)
import GHC.Stack.Types (getCallStack)
import Language.Haskell.TH
import Test.Inspection
import Test.Tasty.HUnit

checkInspection ::
  (HasCallStack) => Result -> Assertion
checkInspection Success {} = pure ()
checkInspection (Failure msg) =
  throwIO $ HUnitFailure (fmap snd $ listToMaybe $ getCallStack callStack) msg

inspecting :: String -> Obligation -> Q Exp
inspecting desc reg =
  [|testCase desc $ checkInspection $(inspectTest reg)|]

data GHCVer = GHC9_8 | GHC9_10 | GHC9_12 | GHC9_14
  deriving (Show, Eq, Ord)

ghcVer :: GHCVer
#if __GLASGOW_HASKELL__ == 914
ghcVer = GHC9_14
#elif __GLASGOW_HASKELL__ == 912
ghcVer = GHC9_12
#elif __GLASGOW_HASKELL__ == 910
ghcVer = GHC9_10
#elif __GLASGOW_HASKELL__ == 908
ghcVer = GHC9_8
#else
ghcVer = error "Coudld not determine GHC Version: __GLASGOW_HASKELL__"
#endif
