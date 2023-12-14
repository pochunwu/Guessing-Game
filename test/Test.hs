-- One of the evaluations is unit testing: the project should pass unit tests that are implemented either manually or using quickcheck.

module Test where

import Guess
import Test.HUnit -- This is a basic unit testing library in Haskell

-- Test cases for the check function
testCheck :: Test
testCheck = TestList [
    "testCorrect" ~: check "hello" "hello" ~?= ("aaaaa", True),
    "testMisplaced" ~: check "helol" "hello" ~?= ("aaabb", False),
    "testIncorrect" ~: check "abcde" "hello" ~?= ("ccccb", False),
    "testMixed" ~: check "holel" "hello" ~?= ("ababb", False)
    ]

-- Function to run all tests
runTests :: IO Counts
runTests = runTestTT testCheck