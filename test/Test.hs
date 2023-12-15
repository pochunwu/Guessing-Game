module Test where

import Guess
import Solver
import Test.HUnit -- This is a basic unit testing library in Haskell
import qualified Data.Map as Map
type FrequencyMapF = Map.Map Char Double

-- Test cases for the check function
testCheck :: Test
testCheck = TestList [
    "testCorrect" ~: check "hello" "hello" ~?= ([Correct, Correct, Correct, Correct, Correct], True),
    "testMisplaced" ~: check "helol" "hello" ~?= ([Correct, Correct, Correct, Misplaced, Misplaced], False),
    "testIncorrect" ~: check "abcde" "hello" ~?= ([Incorrect, Incorrect, Incorrect, Incorrect, Misplaced], False),
    "testMixed" ~: check "holel" "hello" ~?= ([Correct, Misplaced, Correct, Misplaced, Misplaced], False),
    "testEmpty" ~: check "" "" ~?= ([], True),
    "testSingleChar" ~: check "a" "b" ~?= ([Incorrect], False),
    "testLongerStrings" ~: check "haskell" "haswell" ~?= ([Correct, Correct, Correct, Incorrect, Correct, Correct, Correct], False),
    "testAllMisplaced" ~: check "abcde" "edcba" ~?= ([Misplaced, Misplaced, Correct, Misplaced, Misplaced], False),
    "testCorrectAndIncorrect" ~: check "abxyz" "abcde" ~?= ([Correct, Correct, Incorrect, Incorrect, Incorrect], False),
    "testCaseSensitive" ~: check "Hello" "hello" ~?= ([Incorrect, Correct, Correct, Correct, Correct], False),
    "testSpecialCharacters" ~: check "h@ll0" "hello" ~?= ([Correct, Incorrect, Correct, Correct, Incorrect], False)
    ]

testCalculateLetterFrequencies :: Test
testCalculateLetterFrequencies = TestList [
    "testBasicFunctionality" ~: 
        calculateLetterFrequencies ["game", "test"] ~?= 
        Map.fromList [('a',1),('e',2),('g',1),('m',1),('s',1),('t',1)],
    "testEmptyList" ~: 
        calculateLetterFrequencies [] ~?= 
        Map.empty
    ]

testFilterWordsCorrectPlace :: Test
testFilterWordsCorrectPlace = TestList [
    "testBasicFunctionality" ~: 
        filterWordsCorrectPlace ["apple", "ample", "maple"] "a__le" ~?= ["apple", "ample"],
    "testPatternNoMatches" ~: 
        filterWordsCorrectPlace ["apple", "ample", "maple"] "x__le" ~?= []
    ]

almostEqual :: Double -> Double -> Bool
almostEqual x y = abs (x - y) < 1e-6

testScoreWord :: Test
testScoreWord = TestList [
    "testBasicFunctionality" ~: 
        almostEqual (scoreWord "hello" "_el__" dummyFreqMap) 0.8 ~? 
        "Basic functionality test failed: expected 0.8",
    "testEmptyWordPattern" ~: 
        scoreWord "" "" dummyFreqMap ~?= 0.0,
    "testDifferentLengthStrings" ~: 
        scoreWord "hello" "hell" dummyFreqMap ~?= 0.0,
    "testNonMatchingCharacters" ~: 
        scoreWord "hello" "_____" dummyFreqMap ~?= 1.3,
    "testPartiallyMatchingStrings" ~: 
        almostEqual (scoreWord "hello" "he___" dummyFreqMap) 1.0 ~? 
        "Partially matching strings test failed: expected 1.0",
    "testAllNonMatchingCharacters" ~: 
        scoreWord "hello" "abcde" dummyFreqMap ~?= 1.3,
    "testWithSpecialCharacters" ~: 
        scoreWord "hello!" "hello?" dummyFreqMap ~?= 0.0
    ]

-- Replace this with an actual frequency map used in your tests
dummyFreqMap :: FrequencyMapF
dummyFreqMap = Map.fromList [('e', 0.2), ('h', 0.1), ('l', 0.3), ('o', 0.4)]

testFeedbackWord :: Test
testFeedbackWord = TestList [
    "testBasicFunctionality" ~: 
        feedbackWord "test" "next" ~?= "myny",
    "testAllCorrect" ~: 
        feedbackWord "test" "test" ~?= "yyyy",
    "testAllIncorrect" ~: 
        feedbackWord "abcd" "efgh" ~?= "nnnn"
    ]

testNormalizeFrequencies :: Test
testNormalizeFrequencies = TestList [
    "testBasicFunctionality" ~: 
        normalizeFrequencies (Map.fromList [('a', 2), ('b', 1)]) 3 ~?= 
        Map.fromList [('a', 2/3), ('b', 1/3)],
    "testEmptyMap" ~: 
        normalizeFrequencies Map.empty 0 ~?= Map.empty
    ]   

testFilterWordsImpossible :: Test
testFilterWordsImpossible = TestList [
    "testBasicFunctionality" ~: 
        filterWordsImpossible ["apple", "orange", "grape"] ['x', 'z'] ~?= 
        ["apple", "orange", "grape"],
    "testFiltering" ~: 
        filterWordsImpossible ["apple", "orange", "grape"] ['p', 'o'] ~?= 
        [],
    "testFiltering" ~: 
        filterWordsImpossible ["apple", "orange", "grape"] ['p', 'r'] ~?= 
        [],
     "testFiltering" ~: 
    filterWordsImpossible ["apple", "orange", "grape"] ['n'] ~?= 
        ["apple", "grape"]
    ]


-- Function to run all tests
runTests :: IO Counts
runTests = runTestTT $ TestList [testCheck, testCalculateLetterFrequencies, testFilterWordsCorrectPlace, testScoreWord, testFeedbackWord, testNormalizeFrequencies, testFilterWordsImpossible]

