module Solver (calculateEntropyForWords, writeEntropyMapToFile, generateNextGuessList, readFileAsWordScores) where

-- Given the current state of the guess, and the solver will return the best next guess.

import Data.Char (toLower)
import Data.Function (on)
import Data.List (foldl', maximumBy, sortBy, nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Text.Read (readMaybe)
import qualified Data.String

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
    match (w, p) = p == '_' || p == '*' || w == p

-------------------------------------------------------------------------------
-- | Rank possible guesses according to correctly placed letters
-- | Algorithm 1: Sort according to unknown letter frequencies
-------------------------------------------------------------------------------
sortWords :: WordList -> String -> WordList
sortWords words correctPattern = map fst (sortWordsByScore (scoreWords words correctPattern (generateFreqencyMap words)))

sortWordsByScore :: [WordScorePair] -> [WordScorePair]
sortWordsByScore = sortBy (flip compare `on` snd)

-- Scoring function:
scoreWord :: String -> String -> FrequencyMapF -> Double
scoreWord [] [] _ = 0
scoreWord (x : xs) (y : ys) freqMapF
  | x == y = scoreWord xs ys freqMapF
  | otherwise = (Map.findWithDefault 0 x freqMapF) + scoreWord xs ys freqMapF
scoreWord _ _ _ = 0

-- Function to score each word and attach the score
scoreWords :: WordList -> String -> FrequencyMapF -> [WordScorePair]
scoreWords words correctPattern freqMapF = [(word, (scoreWord word correctPattern freqMapF)) | word <- words]

-- Calculate feedback for a guess and an answer
feedbackWord :: String -> String -> String
feedbackWord guess answer = zipWith go guess answer
  where
    go :: Char -> Char -> Char
    go g a
      | g == a          = 'y'
      | g `elem` answer = 'm'
      | otherwise       = 'n'

-- Assuming feedbackPattern is a function that gives a feedback pattern for a pair of words
feedbackCounts :: String -> WordList -> Map String Int
feedbackCounts word words = foldl' updateCount Map.empty words
  where
    updateCount counts w =
      let pattern = feedbackWord word w
      in Map.insertWith (+) pattern 1 counts

-- TOO SLOW
-- feedbackProbabilities :: String -> WordList -> [Double]
-- feedbackProbabilities word words = 
--     let feedbacks = map (feedbackWord word) words
--         uniqueFeedbacks = nub feedbacks
--         feedbackCounts = map (\uf -> length (filter (== uf) feedbacks)) uniqueFeedbacks
--         totalCount = sum feedbackCounts
--     in map (\count -> fromIntegral count / fromIntegral totalCount) feedbackCounts
-- Count feedback patterns and calculate probabilities
feedbackProbabilities :: String -> WordList -> [Double]
feedbackProbabilities word words =
    let fbc = feedbackCounts word words
        fbs = Map.elems fbc
        totalCount = sum fbs
    in map (\count -> fromIntegral count / fromIntegral totalCount) fbs


-- Calculate entropy for a word
calculateEntropy :: [Double] -> Double
calculateEntropy probabilities = -sum (map (\p -> p * logBase 2 p) probabilities)

-- Calculate entropy for each word in the list
calculateEntropyForWords :: WordList -> [WordScorePair]
calculateEntropyForWords words = sortBy (flip compare `on` snd) entropyMap
  where
    entropyMap = map (\word -> (word, calculateEntropy (feedbackProbabilities word words))) words

-- Write Entropy list to file
writeEntropyMapToFile :: FilePath -> [WordScorePair] -> IO ()
writeEntropyMapToFile filename entropyList = do
    let formattedString = unlines [key ++ " " ++ show value | (key, value) <- entropyList]
    writeFile filename formattedString

takeFirstK :: Int -> [a] -> [a]
takeFirstK k xs = take k xs


check :: String -> String -> (String, Map Char [Int], [Char])
check guess answer = (mask, misplacedMap, invalids)
  where
    zipped = zip guess answer
    mask = map (\(g, a) -> go g a) zipped

    misplacedMap = Map.fromListWith (++) [ (g, [i]) | (g, i) <- zip guess [0..], go g (answer !! i) == '_' ]
    invalids = [ g | (g, a) <- zipped, go g a == '*' ]

    go :: Char -> Char -> Char
    go g a
      | g == a          = a
      | g `elem` answer = '_'
      | otherwise       = '*'

-------------------------------------------------------------------------------
-- | Generate a list of best next guesses based on current configuration
-- | Correct pattern is represented as [letter]__[letter]_ where _ means incorrect guess
-- | Misplaced list is represented as a map from char to a list of positions, the positions are invalid placements of the character
-- | Impossible characters are represented as a list of characters, they cannot be presented in the word
-- | Usage: call this function on each round based on the feedback pattern this round. Notice that correctPattern may need to be accumulated across all rounds
-------------------------------------------------------------------------------
generateNextGuessList :: WordList -> String -> Map Char [Int] -> [Char] -> [WordScorePair]
generateNextGuessList words correctPattern misplaced disallowed = calculateEntropyForWords allFiltered
    where
       impossibleFiltered = filterWordsImpossible words disallowed
       incorrectFiltered = filterWordsExistButIncorrectPlace impossibleFiltered misplaced
       allFiltered = filterWordsCorrectPlace incorrectFiltered correctPattern


-- Function to parse a line into a Maybe (String, Double)
parseLine :: String -> Maybe WordScorePair
parseLine line = case Data.String.words line of
    [str, numStr] -> case readMaybe numStr of
        Just num -> Just (str, num)
        Nothing  -> Nothing
    _ -> Nothing

-- Function to read the file and convert it to a list of (String, Double)
readFileAsWordScores :: FilePath -> IO [WordScorePair]
readFileAsWordScores filePath = do
    contents <- readFile filePath
    let linesOfFiles = lines contents
    let maybeTuples = map parseLine linesOfFiles
    return $ map (\(Just x) -> x) (filter (/= Nothing) maybeTuples)

{-
--
>>> let fm = calculateLetterFrequencies ["save", "test", "chest", "player"] in normalizeFrequencies fm (calculateTotalUniqueLetters fm)
fromList [('a',0.1111111111111111),('c',5.555555555555555e-2),('e',0.2222222222222222),('h',5.555555555555555e-2),('l',5.555555555555555e-2),('p',5.555555555555555e-2),('r',5.555555555555555e-2),('s',0.16666666666666666),('t',0.1111111111111111),('v',5.555555555555555e-2),('y',5.555555555555555e-2)]

>>> let constraints = Map.fromList [('a', [0, 2]), ('n', [2]),  ('o', [0])] in filterWordsIncorrectPlace ["save", "test", "chest", "player", "apple", "orange", "banana", "grape", "blueberry", "nation"] constraints
["save","test","chest","blueberry","nation"]

>>> let pattern = "a__le" in filterWordsCorrectPlace ["save", "test", "chest", "player", "apple", "orange", "banana", "grape", "blueberry"] pattern
["apple"]

>>> filterWordsMustContain ["save","test","chest","blueberry","nation", "canon"] "aon"
["nation","canon"]

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
["level","sever","hello"]

>>> generateNextGuessList ["level", "sever", "hello", "desco", "fever", "tesla"] "_e___" (Map.fromList [('l', [0, 4])]) ['v']
[("hello",1.0),("tesla",1.0)]


>>> feedbackWord "test" "next"
"myny"

>>> feedbackWord "test" "dext"
"myny"

>>> feedbackProbabilities "test" ["test", "next", "dext", "nice"]
[0.5,0.25,0.25]

>>> calculateEntropyForWords ["test", "next", "dext", "nice"]
[("next",2.0),("dext",2.0),("test",1.5),("nice",1.5)]

>>> let (correctPattern, misplaced, disallowed) = (check "level" "hello") in (generateNextGuessList ["level", "sever", "hello", "desco", "fever", "tesla"] correctPattern misplaced disallowed)
[("hello",1.0),("tesla",1.0)]
-}
