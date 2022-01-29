{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Shared where
import Control.Exception
import Data.Maybe          (listToMaybe)
import GHC.Stack           (callStack)
import GHC.Stack.Types     (getCallStack)
import Language.Haskell.TH
import Test.Inspection
import Test.Tasty.HUnit

checkInspection
  :: HasCallStack => Result -> Assertion
checkInspection Success{} = pure ()
checkInspection (Failure msg) =
  throwIO $ HUnitFailure (fmap snd $ listToMaybe $ getCallStack callStack) msg

inspecting :: String -> Obligation -> Q Exp
inspecting desc reg =
  [|testCase desc $ checkInspection $(inspectTest reg)|]

data GHCVer = GHC8_8 | GHC8_10 | GHC9_0 | GHC9_2
  deriving (Show, Eq, Ord)

ghcVer :: GHCVer
#if __GLASGOW_HASKELL__ == 902
ghcVer = GHC9_2
#elif __GLASGOW_HASKELL__ == 900
ghcVer = GHC9_0
#elif __GLASGOW_HASKELL__ == 810
ghcVer = GHC8_10
#elif __GLASGOW_HASKELL__ == 808
ghcVer = GHC8_8
#else
ghcVer = error "Coudld not determine GHC Version: __GLASGOW_HASKELL__"
#endif
