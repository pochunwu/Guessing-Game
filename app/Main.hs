{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Main where

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes.Color
import Brick.Util
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
  , padLeft
  , padRight
  )
import Brick.Widgets.Border (border)
import Brick.Widgets.Border.Style (unicode, unicodeBold)
import Brick.Widgets.Center (center, hCenter)

import Guess
import Choose

data GameStatus =       
      Fresh
    | Correct
    | Incorrect
    deriving (Eq, Show)

data State = State
  { _sWords :: [String],
    _sWord :: String,
    _sWordSize :: Int,
    _sInput :: String,
    _sStatus :: String,
    _sAttemps :: [String],
    _sAttempsStatus :: [[Guess.State]],
    _sGameStatus :: GameStatus
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
        _sAttemps = [],
        _sAttempsStatus = [],
        _sGameStatus = Main.Fresh
    }


app :: M.App Main.State AppEvent ()
app =
  M.App
    { M.appDraw = draw,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = handleEvent,
      M.appStartEvent = return (),
      M.appAttrMap = const $ attrMap V.defAttr guessAppAttrMap
    }

guessAppAttrMap :: [(A.AttrName, V.Attr)]
guessAppAttrMap = [ 
    (A.attrName "highlight", fg yellow), 
    (A.attrName "warning", fg yellow),
    (A.attrName "ongoing", fg green),
    (A.attrName "correct", fg green),
    (A.attrName "misplaced_char", fg yellow)]

data AppEvent = Dummy deriving Show

draw :: Main.State -> [T.Widget ()]
draw s =
    [center . hBox $ [
        vBox $ [drawGame s, drawInput s],
        vBox $ [drawStatus s]
      ]
    ]

drawStatus :: Main.State -> T.Widget ()
drawStatus s =
  withBorderStyle unicode $ border (vBox [ 
      str ("Status: " ++ s^.sWord),
      if s^.sGameStatus == Main.Fresh
        -- green text
        then withAttr (A.attrName "ongoing") $ str "On going"
      else if s^.sGameStatus == Main.Correct
        then withAttr (A.attrName "correct") $ str "Correct"
      else if s^.sGameStatus == Main.Incorrect
        then withAttr (A.attrName "warning") $ str "Incorrect"
      else
        str "Unknown"
  ])

drawGame :: Main.State -> T.Widget ()
drawGame s =
  withBorderStyle unicode $ border (vBox [ 
      str "Game: ",
      vBox (drawAttempts (s^.sAttemps) (s^.sAttempsStatus))
  ])
  where
    drawAttempts :: [String] -> [[Guess.State]] -> [T.Widget ()]
    drawAttempts l wordleStatesList =
      map f $ zip l' w'
      where
        l' = l ++ replicate (s^.sWordSize + 1 - length l) ""
        w' = wordleStatesList ++ replicate (s^.sWordSize + 1 - length wordleStatesList) []
        f :: (String, [Guess.State]) -> T.Widget ()
        f (attempStr, wordleStates) = do
          if length attempStr == 0
            then
              hBox (replicate (s^.sWordSize) (drawCharWithBorder ' '))
            else
              hBox (map drawCharWithState $ zip attempStr wordleStates)
              where
                drawCharWithState :: (Char, Guess.State) -> T.Widget ()
                drawCharWithState (c, s) =
                  case s of
                    Guess.Correct   -> 
                      withAttr (A.attrName "correct") (drawCharWithBorder c)
                    Guess.Misplaced -> 
                      withAttr (A.attrName "misplaced_char") (drawCharWithBorder c)
                    Guess.Incorrect -> 
                      drawCharWithBorder c

drawCharWithBorder :: Char -> T.Widget ()
drawCharWithBorder c = do
  withBorderStyle unicode $
    border $
      padLeftRight 1 $
        str [c]

drawGuessList :: Main.State -> [Char] -> T.Widget ()
drawGuessList s l = do
  hBox $ map drawCharWithBorder l'
  where 
    currentLen = length l
    l' = l ++ replicate (s^.sWordSize - currentLen) ' '
        
drawInput :: Main.State -> T.Widget ()
drawInput s =
  withBorderStyle unicode $
    border $
      vBox [ 
          hBox [
            str "Guess"
          ],
          hBox [
              drawGuessList s $ s^.sInput
          ]
      ]

handleEvent :: T.BrickEvent () AppEvent -> T.EventM () Main.State ()
handleEvent e =
  case e of
    T.VtyEvent (V.EvKey V.KEsc []) -> M.halt
    T.VtyEvent (V.EvKey (V.KChar c) []) -> do
      sGameStatus .= Main.Fresh
      wordsize <- use sWordSize
      input <- use sInput
      if length input < wordsize
        then do
          sInput %= (++ [c])
        else do
          -- we have collect all the input, now we can check
          return ()
    T.VtyEvent (V.EvKey V.KEnter []) -> do
      input <- use sInput
      maxWordSize <- use sWordSize
      word <- use sWord
      if length input == maxWordSize then do
        sAttemps %= (++ [input])
        sInput .= ""
        let (wordle, result) = check input word
        sAttempsStatus %= (++ [wordle])
        if result
          then do
            sGameStatus .= Main.Correct
          else do
            sGameStatus .= Main.Incorrect
        M.invalidateCache
      else do
        return ()
      -- let (wordle, result) = check input word
      -- sAttemps %= (++ [input])
      -- sInput .= ""
      -- if result
      --   then do
      --     sGameStatus .= Guess.Correct
      --   else do
      --     sGameStatus .= Guess.Incorrect
    
handleEvent _ = return ()

main :: IO ()
main = do
  s <- appMain
  return ()
