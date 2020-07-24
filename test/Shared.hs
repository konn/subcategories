{-# LANGUAGE TemplateHaskell #-}
module Shared where
import Language.Haskell.TH
import Test.Hspec
import Test.Inspection

checkInspection
  :: Result -> Expectation
checkInspection Success{} = pure ()
checkInspection (Failure msg) =
  fail msg

inspecting :: String -> Obligation -> Q Exp
inspecting desc reg =
  [|it desc $ checkInspection $(inspectTest reg)|]
