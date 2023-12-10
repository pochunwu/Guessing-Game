module Countdown (countdown) where

import Control.Concurrent (threadDelay)

countdown :: Int -> IO ()
countdown 0 = putStrLn "Time's up!"
countdown n = do
  putStrLn $ "Seconds left: " ++ show n
  threadDelay 1000000 -- Delay for 1 second (1000000 microseconds)
  countdown (n - 1)