{-# LANGUAGE TemplateHaskell #-}
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
