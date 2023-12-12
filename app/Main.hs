{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Main where

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl
import Control.Monad (void, forever)
import Control.Concurrent (threadDelay, forkIO)
import Brick.AttrMap
  ( attrMap
  )
import Brick.Types
  ( Widget
  , EventM
  , BrickEvent(..)
  )
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , withAttr
  , vLimit
  , hLimit
  , hBox
  , vBox
  , updateAttrMap
  , withBorderStyle
  , txt
  , str
  , padLeftRight
  )

import Guess
import Choose

data State = State
  { _sWords :: [String],
    _sWord :: String,
    _sWordSize :: Int,
    _sInput :: String,
    _sStatus :: String,
    _sGameStatus :: Guess.State
  }
  deriving (Show, Eq)
makeLenses ''Main.State

appMain :: IO Main.State
appMain = do
  M.defaultMain app =<< initState

initState :: IO Main.State
initState = do
    putStrLn "Welcome to the Guessing Game 🎉"
    putStrLn "Please choose what to guess: "
    putStrLn "1 - Words (5-letter)"
    putStrLn "2 - Animals"
    putStrLn "3 - US cities"
    putStrLn "4 - Names"
    topics <- readLn :: IO Int
    word <- genRandomWord topics
    putStrLn "Choose Difficulty: "
    putStrLn "3 - Hard"
    putStrLn "5 - Medium"
    putStrLn "7 - Easy"
    difficulty <- readLn :: IO Int
    putStrLn $ "Word Length: " ++ show (length word)
    putStrLn $ "Difficulty: " ++ show difficulty
    return $ Main.State {
        _sWords = [],
        _sWord = word,
        _sWordSize = length word,
        _sInput = "",
        _sStatus = "",
        _sGameStatus = Guess.Fresh
    }


app :: M.App Main.State AppEvent ()
app =
  M.App
    { M.appDraw = draw,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = handleEvent,
      M.appStartEvent = return (),
      M.appAttrMap = const $ attrMap V.defAttr []
    }

data AppEvent = Dummy deriving Show

draw :: Main.State -> [T.Widget ()]
draw s =
    [hBox [drawGame s, drawInput s]]

drawGame :: Main.State -> T.Widget ()
drawGame s =
    str $ show (s^.sGameStatus)

drawInput :: Main.State -> T.Widget ()
drawInput s =
    str $ s^.sInput

handleEvent :: T.BrickEvent () AppEvent -> T.EventM () Main.State ()
handleEvent e =
  case e of
    T.VtyEvent (V.EvKey V.KEsc []) -> M.halt
    T.VtyEvent (V.EvKey (V.KChar c) []) -> do
        sInput %= (++ [c])
    
handleEvent _ = return ()

main :: IO ()
main = do
  s <- appMain
  return ()
