{-# LANGUAGE TemplateHaskell #-}
module Shared where
import Control.Exception
import Language.Haskell.TH
import Test.Inspection
import Test.Tasty.HUnit

checkInspection
  :: HasCallStack => Result -> Assertion
checkInspection Success{} = pure ()
checkInspection (Failure msg) =
  throwIO $ HUnitFailure Nothing msg

inspecting :: String -> Obligation -> Q Exp
inspecting desc reg =
  [|testCase desc $ checkInspection $(inspectTest reg)|]
