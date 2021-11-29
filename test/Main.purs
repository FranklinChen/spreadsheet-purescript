module Test.Main where

import Prelude

import Effect (Effect)
import Test.Unit.Main (runTest)

import Test.Spreadsheet (moduleSuite)

main :: Effect Unit
main = runTest do
  moduleSuite
