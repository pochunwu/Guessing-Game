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
    input <- readLn :: IO Int
    result <- genRandomWord input
    putStrLn $ "Word Length: " ++ show (length result)
    play result 5

