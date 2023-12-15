module Choose (genRandomWord, genRandomWordString) where

import System.Random ( randomRIO )

genRandomWord :: Int -> IO String
genRandomWord  n
    | n == 1 = do
        strings <- readFileToList "data/wordle5.txt"
        getRandomElement strings
    | n == 2 = do
        strings <- readFileToList "data/animals.txt"
        getRandomElement strings
    | n == 3 = do
        strings <- readFileToList "data/citiesUS.txt"
        getRandomElement strings
    | n == 4 = do
        strings <- readFileToList "data/names.txt"
        getRandomElement strings
    | otherwise = error "Invalid Input"

genRandomWordString :: Int -> String
genRandomWordString n = do
    genRandomWordString n

readFileToList :: String -> IO [String]
readFileToList fileName = lines <$> readFile fileName

getRandomElement :: [a] -> IO a
getRandomElement xs = do
  randomIndex <- randomRIO (0, length xs - 1)
  return (xs !! randomIndex)
