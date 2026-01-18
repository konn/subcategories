{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Shared where

import Control.Exception
import Data.Maybe (listToMaybe)
import GHC.Stack (callStack)
import GHC.Stack.Types (getCallStack)
import Language.Haskell.TH
import Test.Inspection
import Test.Tasty (TestTree)
import Test.Tasty.ExpectedFailure (expectFailBecause)
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

prettyGHCVer :: GHCVer -> String
prettyGHCVer GHC9_8 = "GHC 9.8"
prettyGHCVer GHC9_10 = "GHC 9.10"
prettyGHCVer GHC9_12 = "GHC 9.12"
prettyGHCVer GHC9_14 = "GHC 9.14"

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

expectFailSinceBecause :: GHCVer -> String -> TestTree -> TestTree
expectFailSinceBecause ver reason =
  if ghcVer >= ver
    then expectFailBecause (reason <> " (since " <> prettyGHCVer ver <> ")")
    else id
