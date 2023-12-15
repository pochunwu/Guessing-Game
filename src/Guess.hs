module Guess (State(..), check) where

import Data.Char ( toLower )

data State =
      Fresh
    | Correct
    | Misplaced
    | Incorrect
    | Normal
    deriving (Eq, Show)

-- showState :: State -> Char
-- showState Correct   = '🟩'
-- showState Misplaced = '🟨'
-- showState Incorrect = '🟥'

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


