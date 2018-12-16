module Test.Pcf.V2.SimpleTest where

import           Pcf.V2.Types
import           Test.Tasty
import           Test.Tasty.HUnit

test_something :: TestTree
test_something = testCase "something" (1 @?= 1)
