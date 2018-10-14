module Test.Pcf.SimpleTest where

import Test.Tasty
import Test.Tasty.HUnit

test_something :: TestTree
test_something = testCase "something" $ do
    let actual = 1 + 1
        expected = 2
    actual @?= expected
