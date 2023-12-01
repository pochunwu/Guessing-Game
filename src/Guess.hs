module Guess where

import Data.Char ( isSpace, toLower )

data State =
      Correct
    | Misplaced
    | Incorrect

showState :: State -> Char
showState Correct   = '🟩'
showState Misplaced = '🟨'
showState Incorrect = '🟥'

maxAttempts :: Int
maxAttempts = 5

play :: String -> Int -> IO()
play answer = go
  where
    go :: Int -> IO()
    go 0 = putStrLn "Sorry, you lose 😭"
    go n = do
      putStrLn $ "Attempt(s): " ++ show (maxAttempts - n + 1) ++ "/" ++ show maxAttempts ++  "\n🙏 Please make your guess: "
      input <- getLine
      let guess = map toLower (trim input)
      if length guess /= length answer
        then do
          putStrLn "The word length of your guess does not match the answer 🥲 . \n🥹 Please try again:"
          go n
      else do
          let (wordle, result) = check guess answer
          putStrLn wordle
          if result
            then do
              putStrLn "👏 Congratulation! You win 🎉"
            else do
              go (n - 1)

check :: String -> String -> (String, Bool)
check guess answer = (zipWith go guess answer, guess == answer)
  where
    go :: Char -> Char -> Char
    go g a
      | g == a          = showState Correct
      | g `elem` answer = showState Misplaced
      | otherwise       = showState Incorrect

-- Helper functions
trim :: String -> String
trim = unwords . words


