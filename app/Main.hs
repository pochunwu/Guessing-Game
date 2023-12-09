module Main (main) where

import Guess (play)
import Choose (genRandomWord)

main :: IO ()
main = do
    putStrLn "Welcome to the Guessing Game 🎉"
    putStrLn "Please choose what to guess: "
    putStrLn "1 - Words (5-letter)"
    putStrLn "2 - Animals"
    putStrLn "3 - US cities"
    putStrLn "4 - Names"
    topics <- readLn :: IO Int
    result <- genRandomWord topics
    putStrLn "Choose Difficulty: "
    putStrLn "3 - Hard"
    putStrLn "5 - Medium"
    putStrLn "7 - Easy"
    difficulty <- readLn :: IO Int
    putStrLn $ "Word Length: " ++ show (length result)
    play result difficulty difficulty

