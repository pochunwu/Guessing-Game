{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Main where

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Core as C
import Graphics.Vty.Attributes.Color
import Brick.Util
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl
import Control.Monad (void, forever, liftM2)
import Control.Concurrent (threadDelay, forkIO)
import Control.DeepSeq (deepseq)
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
  , padTop
  )
import Brick.Widgets.Border (border)
import Brick.Widgets.Border.Style (unicode, unicodeBold)
import Brick.Widgets.Center (center, hCenter, vCenter)

import qualified Data.Map as Map

import Guess
import Choose

data GameStatus =       
      Fresh
    | Correct
    | Incorrect
    | Lose
    deriving (Eq, Show)

data State = State
  { _sWords :: [String],
    _sWord :: String,
    _sWordSize :: Int,
    _sInput :: String,
    _sStatus :: String,
    _sAttemps :: [String],
    _sAttempsStatus :: [[Guess.State]],
    _sKeyboardState :: [(Char, Guess.State)], -- Map.Map Char Guess.State,
    _sGameStatus :: GameStatus,
    _sScreen :: Int,
    _sSelectedMode :: Int,
    _sSelectedDifficulty :: Int
  }
  deriving (Show, Eq)
makeLenses ''Main.State

initState :: Int -> Int -> Int -> IO Main.State
initState scr mode difficulty = do
    word <- genRandomWord mode
    let wordsize = length word
    return $ Main.State {
        _sWords = [],
        _sWord = word, -- word,
        _sWordSize = wordsize, --length word,
        _sInput = "",
        _sStatus = "",
        _sAttemps = [],
        _sAttempsStatus = [],
        _sGameStatus = Main.Fresh,
        _sKeyboardState = [
          ('a', Guess.Normal),
          ('b', Guess.Normal),
          ('c', Guess.Normal),
          ('d', Guess.Normal),
          ('e', Guess.Normal),
          ('f', Guess.Normal),
          ('g', Guess.Normal),
          ('h', Guess.Normal),
          ('i', Guess.Normal),
          ('j', Guess.Normal),
          ('k', Guess.Normal),
          ('l', Guess.Normal),
          ('m', Guess.Normal),
          ('n', Guess.Normal),
          ('o', Guess.Normal),
          ('p', Guess.Normal),
          ('q', Guess.Normal),
          ('r', Guess.Normal),
          ('s', Guess.Normal),
          ('t', Guess.Normal),
          ('u', Guess.Normal),
          ('v', Guess.Normal),
          ('w', Guess.Normal),
          ('x', Guess.Normal),
          ('y', Guess.Normal),
          ('z', Guess.Normal)
        ], -- Map.fromList $ zip ['a'..'z'] (repeat Guess.Incorrect)
        _sScreen = scr, -- 0 - mode selection, 1 - game
        _sSelectedMode = 0,
        _sSelectedDifficulty = difficulty
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
    (A.attrName "misplaced_char", fg yellow),
    (A.attrName "incorrect", fg red),
    (A.attrName "mode_selected", fg green),
    (A.attrName "difficulty_selected", fg yellow)]

data AppEvent = Dummy deriving Show

draw :: Main.State -> [T.Widget ()]
draw s =
    case s ^. sScreen of
        0 -> [drawModeSelection s]
        1 -> [drawDifficultySelection s]
        2 -> [drawGameView s]
        _ -> [drawGameView s]

drawGameView :: Main.State -> T.Widget ()
drawGameView s = 
  center . hBox $ [
      vBox $ [drawGame s, drawInput s],
      vBox $ [drawStatus s]
  ]

drawModeSelection :: Main.State -> T.Widget ()
drawModeSelection s =
  withBorderStyle unicode $ border (padLeft (C.Pad 1) $ vBox [ 
      str "Use Arrow to Choose Mode: ",
      addAttr 0 $ str "1 - Words (5-letter)",
      addAttr 1 $ str "2 - Animals",
      addAttr 2 $ str "3 - US cities",
      addAttr 3 $ str "4 - Names"
  ])
  where
    addAttr mode = 
      if mode == s^.sSelectedMode
        then
          withAttr (A.attrName "mode_selected")
        else
          id

drawDifficultySelection :: Main.State -> T.Widget ()
drawDifficultySelection s =
  withBorderStyle unicode $ border (padLeft (C.Pad 1) $ vBox [ 
      str "Use Arrow to Choose Difficulty: ",
      addAttr 0 $ str "3 - Hard",
      addAttr 1 $ str "5 - Medium",
      addAttr 2 $ str "7 - Easy"
  ])
  where
    addAttr mode = 
      if mode == s^.sSelectedDifficulty
        then
          withAttr (A.attrName "difficulty_selected")
        else
          id

drawUsage :: Main.State -> T.Widget ()
drawUsage s =
  (vBox [ 
      str "Usage: ",
      str "Press ESC to quit",
      str "Press ENTER to submit",
      str "Press DEL to delete",
      str "Press any other key to input"
  ])

drawStatus :: Main.State -> T.Widget ()
drawStatus s =
  withBorderStyle unicode $ border (padLeft (C.Pad 1) $ vBox [ 
      str ("Status: " ++ s^.sWord),
      if s^.sGameStatus == Main.Fresh
        -- green text
        then withAttr (A.attrName "ongoing") $ str "On going"
      else if s^.sGameStatus == Main.Correct
        then withAttr (A.attrName "correct") $ str "Correct"
      else if s^.sGameStatus == Main.Incorrect
        then withAttr (A.attrName "warning") $ str "Incorrect"
      else if s^.sGameStatus == Main.Lose
        then withAttr (A.attrName "incorrect") $ str "Lose"
      else
        str "Unknown",
      padTop (C.Pad 1) $ drawKeyboard s,
      padTop (C.Pad 1) $ drawUsage s
  ])

rawKeyboard :: [String]
rawKeyboard = ["qwertyuiop", "asdfghjkl", "zxcvbnm"]
drawKeyboard :: Main.State -> T.Widget ()
drawKeyboard s = do
  -- vBox $ map drawCharList rawKeyboard
  -- vertically align the keyboard
  hLimit 30 $ vBox $ map hCenter (map drawCharList rawKeyboard)
  where
    drawCharList l = do
      padRight (C.Pad 1) $ hBox $ map drawChar l
    drawChar c = do
      let state = lookup c (s^.sKeyboardState)
      case state of
        Just Guess.Correct -> 
          withAttr (A.attrName "correct") (padLeft (C.Pad 1) $ str [c])
        Just Guess.Misplaced -> 
          withAttr (A.attrName "misplaced_char") (padLeft (C.Pad 1) $ str [c])
        Just Guess.Incorrect -> 
          withAttr (A.attrName "incorrect") $ padLeft (C.Pad 1) $ str [c]
        Just Guess.Normal -> 
          id padLeft (C.Pad 1) $ str [c]

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
        l' = l ++ replicate ((s^.sSelectedDifficulty * 2 + 1) - length l) ""
        w' = wordleStatesList ++ replicate ((s^.sSelectedDifficulty * 2 + 1) - length wordleStatesList) []
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
                    _ -> 
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
            str "Guess "
          ],
          hBox [
              drawGuessList s $ s^.sInput
          ]
      ]

unwrapState :: Maybe Guess.State -> Guess.State
unwrapState (Just a) = a
unwrapState Nothing = Guess.Normal

handleEnter :: T.BrickEvent () AppEvent -> T.EventM () Main.State ()
handleEnter _ = do
  screen <- use sScreen
  -- 0, select wordset
  -- 1, select difficulty
  -- 2, start game
  if screen == 0 then do
    M.halt
  else if screen == 1 then do
    M.halt
  else do
    input <- use sInput
    attemps <- use sAttemps
    difficulty <- use sSelectedDifficulty
    word <- use sWord
    if length input == length word && length attemps < difficulty * 2 + 1 then do
      sAttemps %= (++ [input])
      attemps <- use sAttemps
      sInput .= ""
      let (wordle, result) = check input word
      sAttempsStatus %= (++ [wordle])
      keyboardState <- use sKeyboardState
      if result
        then do
          sGameStatus .= Main.Correct
        else do
          if length attemps == difficulty * 2 + 1 then
            sGameStatus .= Main.Lose
          else
            sGameStatus .= Main.Incorrect
      let currentState = zip input wordle
      -- update keyboard state, if the char is already correct, then don't update
      sKeyboardState %= map (\(c, s) -> if s == Guess.Correct then (c, s) else (c, unwrapState $ lookup c currentState))
      M.invalidateCache
    else do
      return ()

handleEvent :: T.BrickEvent () AppEvent -> T.EventM () Main.State ()
handleEvent e =
  case e of
    T.VtyEvent (V.EvKey V.KEsc []) -> M.halt
    T.VtyEvent (V.EvKey (V.KFun 5) []) -> do
      M.halt
    T.VtyEvent (V.EvKey (V.KChar c) []) -> do
      currentStatus <- use sGameStatus
      if currentStatus == Main.Lose || currentStatus == Main.Correct
        then do
          return ()
        else do
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
      handleEnter e
    T.VtyEvent (V.EvKey V.KBS []) -> do
      -- remove the last char
      input <- use sInput
      if length input > 0
        then do
          sInput .= take (length input - 1) input
        else do
          return ()
    -- up and down arrow key
    T.VtyEvent (V.EvKey V.KUp []) -> do
      currentMode <- use sSelectedMode
      currentDifficulty <- use sSelectedDifficulty
      currentScreen <- use sScreen
      if currentScreen == 0 then do
        if currentMode > 0
          then do
            sSelectedMode -= 1
          else do
            return ()
      else if currentScreen == 1 then do
        if currentDifficulty > 0
          then do
            sSelectedDifficulty -= 1
          else do
            return ()
      else do
        return ()
    T.VtyEvent (V.EvKey V.KDown []) -> do
      currentMode <- use sSelectedMode
      currentDifficulty <- use sSelectedDifficulty
      currentScreen <- use sScreen
      if currentScreen == 0 then do
        if currentMode < 3
          then do
            sSelectedMode += 1
          else do
            return ()
      else if currentScreen == 1 then do
        if currentDifficulty < 2
          then do
            sSelectedDifficulty += 1
          else do
            return ()
      else do
        return ()
    _ -> return ()

    
handleEvent _ = return ()

appModeSelection :: IO Main.State
appModeSelection = do
  M.defaultMain app =<< initState 0 1 1 

appDifficultySelection :: IO Main.State
appDifficultySelection = do
  M.defaultMain app =<< initState 1 1 1

appMain :: Int -> Int -> Int -> IO Main.State
appMain scr mode difficulty = do
  M.defaultMain app =<< initState scr mode difficulty

main :: IO ()
main = do 
  s0 <- appModeSelection
  s1 <- appDifficultySelection
  s2 <- appMain 2 (s0^.sSelectedMode + 1) (s1^.sSelectedDifficulty + 1)

  return ()
