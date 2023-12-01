module Main (main) where

import Guess (play)

main :: IO ()
main = do
    play "hello" 5

