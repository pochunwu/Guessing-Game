module Solver where

-- Given the current state of the guess, and the solver will return the best next guess.

import Data.Char (toLower)
import Data.List (foldl', maximumBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)

type WordList = [String]

type FrequencyMap = Map Char Int

type FrequencyMapF = Map Char Float

type ScoreMap = Map String Int

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

-- -------------------------------------------------------------------------------
-- -- | Filter possible words according to correctly placed letters
-- -------------------------------------------------------------------------------
-- sortWords :: WordList -> WordList
-- sortWords words pattern =
--      [word | word <- words, matchesPattern word pattern]
--   where
--     matchesPattern word patt =
--         length word == length patt && all match (zip word patt)
--     match (w, p) = p == '_' || w == p

{-
--
>>> let fm = calculateLetterFrequencies ["save", "test", "chest", "player"] in normalizeFrequencies fm (calculateTotalUniqueLetters fm)
fromList [('a',0.11111111),('c',5.5555556e-2),('e',0.22222222),('h',5.5555556e-2),('l',5.5555556e-2),('p',5.5555556e-2),('r',5.5555556e-2),('s',0.16666667),('t',0.11111111),('v',5.5555556e-2),('y',5.5555556e-2)]

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

--
-}