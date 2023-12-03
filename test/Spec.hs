module Main where

import Test.HUnit
import qualified Test -- import your test module

main :: IO ()
main = do
    _ <- Test.runTests
    return ()
