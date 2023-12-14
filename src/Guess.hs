module Guess (play, check, State (..)) where

import Data.Char ( toLower )

data State =
      Fresh
    | Correct
    | Misplaced
    | Incorrect
    deriving (Eq, Show)

-- showState :: State -> Char
-- showState Correct   = '🟩'
-- showState Misplaced = '🟨'
-- showState Incorrect = '🟥'

play :: String -> Int -> Int -> IO()
play answer = go
  where
    go :: Int -> Int -> IO()
    go _ 0 = putStrLn $ "Sorry, you lose 😭 \nThe answer is " ++ show answer ++ " 🌟"
    go maxAttempts n = do
      putStrLn $ "Attempt(s): " ++ show (maxAttempts - n + 1) ++ "/" ++ show maxAttempts ++  "\n🙏 Please make your guess: "
      input <- getLine
      let guess = map toLower (trim input)
      if length guess /= length answer
        then do
          putStrLn "The word length of your guess does not match the answer 🥲 ."
          go maxAttempts (n - 1)
      else do
          let (wordle, result) = check guess answer
          print wordle
          if result
            then do
              putStrLn "👏 Congratulation! You win 🎉"
            else do
              go maxAttempts (n - 1)

check :: String -> String -> ([State], Bool)
check guess answer = (zipWith go guess answer, guess == answer)
  where
    go :: Char -> Char -> State
    go g a
      | g == a          = Correct
      | g `elem` answer = Misplaced
      | otherwise       = Incorrect

-- Helper functions
trim :: String -> String
trim = unwords . words


