module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)

main = run [consoleReporter] do
  describe "a test" do
    it "work" do
      false `shouldEqual` true
