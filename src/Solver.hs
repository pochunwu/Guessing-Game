module Solver where

-- Given the current state of the guess, and the solver will return the best next guess.

import Data.Char (toLower)
import Data.Function (on)
import Data.List (foldl', maximumBy, sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Prelude hiding (words)

type WordList = [String]

type FrequencyMap = Map Char Int

type FrequencyMapF = Map Char Double

type WordScorePair = (String, Double)

calculateTotalUniqueLetters :: FrequencyMap -> Int
calculateTotalUniqueLetters freqMap = sum (Map.elems freqMap)

calculateLetterFrequencies :: WordList -> FrequencyMap
calculateLetterFrequencies = foldl' f Map.empty
  where
    f acc word = foldl' updateFrequencyMap acc (uniqueLetters word)
    updateFrequencyMap freqMap letter = Map.insertWith (+) letter 1 freqMap
    uniqueLetters = foldl' (\acc c -> if c `elem` acc then acc else c : acc) ""

normalizeFrequencies :: FrequencyMap -> Int -> FrequencyMapF
normalizeFrequencies freqMap total = Map.map (\v -> (fromIntegral v) / (fromIntegral total)) freqMap

generateFreqencyMap :: WordList -> FrequencyMapF
generateFreqencyMap wordList = let fm = calculateLetterFrequencies wordList 
    in normalizeFrequencies fm (calculateTotalUniqueLetters fm)

-------------------------------------------------------------------------------

-- | Filter possible words according to impossible to appear letters

-------------------------------------------------------------------------------
filterWordsImpossible :: WordList -> [Char] -> WordList
filterWordsImpossible words disallowed =
  [word | word <- words, all (`notElem` disallowed) word]

-------------------------------------------------------------------------------

-- | Filter possible words that cannot be presented in given places per letter

-------------------------------------------------------------------------------
filterWordsIncorrectPlace :: WordList -> Map Char [Int] -> WordList
filterWordsIncorrectPlace words constraints =
  [word | word <- words, all (\(index, char) -> notElem index (Map.findWithDefault [] char constraints)) (zip [0 ..] word)]

-------------------------------------------------------------------------------

-- | Filter possible words according to incorrectly placed letters

-------------------------------------------------------------------------------
filterWordsExistButIncorrectPlace :: WordList -> Map Char [Int] -> WordList
filterWordsExistButIncorrectPlace words constraints =
  filterWordsMustContain (filterWordsIncorrectPlace words constraints) (Map.keys constraints)

-------------------------------------------------------------------------------

-- | Filter possible words that must contain letters in the list

-------------------------------------------------------------------------------
filterWordsMustContain :: WordList -> [Char] -> WordList
filterWordsMustContain words letters =
  [word | word <- words, all (`elem` word) letters]

-------------------------------------------------------------------------------

-- | Filter possible words according to correctly placed letters

-------------------------------------------------------------------------------
filterWordsCorrectPlace :: WordList -> String -> WordList
filterWordsCorrectPlace words pattern =
  [word | word <- words, matchesPattern word pattern]
  where
    matchesPattern word patt =
      length word == length patt && all match (zip word patt)
    match (w, p) = p == '_' || w == p

-------------------------------------------------------------------------------

-- | Rank possible guesses according to correctly placed letters
-- | Algorithm 1: Sort according to unknown letter frequencies

-------------------------------------------------------------------------------
sortWords :: WordList -> String -> WordList
sortWords words correctPattern = map fst (sortWordsByScore (scoreWords words correctPattern (generateFreqencyMap words)))

sortWordsByScore :: [WordScorePair] -> [WordScorePair]
sortWordsByScore = sortBy (compare `on` snd)

-- Scoring function:
scoreWord :: String -> String -> FrequencyMapF -> Double
scoreWord [] [] _ = 0
scoreWord (x : xs) (y : ys) freqMapF
  | x == y = scoreWord xs ys freqMapF
  | otherwise = (Map.findWithDefault 0 x freqMapF) + scoreWord xs ys freqMapF
scoreWord _ _ _ = 0

-- Function to score each word and attach the score
scoreWords :: [String] -> String -> FrequencyMapF -> [WordScorePair]
scoreWords words correctPattern freqMapF = [(word, (scoreWord word correctPattern freqMapF)) | word <- words]

{-
--
>>> let fm = calculateLetterFrequencies ["save", "test", "chest", "player"] in normalizeFrequencies fm (calculateTotalUniqueLetters fm)
fromList [('a',0.1111111111111111),('c',5.555555555555555e-2),('e',0.2222222222222222),('h',5.555555555555555e-2),('l',5.555555555555555e-2),('p',5.555555555555555e-2),('r',5.555555555555555e-2),('s',0.16666666666666666),('t',0.1111111111111111),('v',5.555555555555555e-2),('y',5.555555555555555e-2)]

>>> let constraints = Map.fromList [('a', [0, 2]), ('n', [2]),  ('o', [0])] in filterWordsIncorrectPlace ["save", "test", "chest", "player", "apple", "orange", "banana", "grape", "blueberry", "nation"] constraints
["save","test","chest","blueberry","nation"]

>>> let pattern = "a__le" in filterWordsCorrectPlace ["save", "test", "chest", "player", "apple", "orange", "banana", "grape", "blueberry"] pattern
["apple"]

>>> filterWordsMustContain ["save","test","chest","blueberry","nation"] "aon"
["nation"]

>>> let constraints = Map.fromList [('a', [0, 2]), ('n', [2]),  ('o', [0])] in (filterWordsIncorrectPlace ["save", "test", "chest", "player", "apple", "orange", "banana", "grape", "blueberry",  "nation"] constraints)
["save","test","chest","blueberry","nation"]

>>> let constraints = Map.fromList [('a', [0, 2]), ('n', [2]),  ('o', [0])] in filterWordsExistButIncorrectPlace ["save", "test", "chest", "player", "apple", "orange", "banana", "grape", "blueberry",  "nation"] constraints
["nation"]

>>> generateFreqencyMap ["level", "sever", "hello"]
fromList [('e',0.2727272727272727),('h',9.090909090909091e-2),('l',0.18181818181818182),('o',9.090909090909091e-2),('r',9.090909090909091e-2),('s',9.090909090909091e-2),('v',0.18181818181818182)]

>>> scoreWord "hello" "_el__" (generateFreqencyMap ["level", "sever", "hello"])
0.36363636363636365

>>> scoreWords ["level", "sever", "hello"] "_e___" (generateFreqencyMap ["level", "sever", "hello"])
[("level",0.8181818181818181),("sever",0.6363636363636364),("hello",0.5454545454545454)]

>>> sortWords ["level", "sever", "hello"]  "_e___"
["hello","sever","level"]

--
-}
