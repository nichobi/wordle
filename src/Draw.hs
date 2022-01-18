module Draw (drawUI, myAttrMap) where

import Util
import WordleComp (wordleComp)

import Data.Maybe (fromMaybe)

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , defaultMain, showFirstCursor, continueWithoutRedraw
  , continue, halt
  , str, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding (Max)
  , attrMap, withAttr, AttrName, attrName, bg
  )
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import qualified Brick.Widgets.Core         as W
import qualified Graphics.Vty               as V

resultToAttr :: Result -> AttrName
resultToAttr = attrName . show

myAttrMap :: AttrMap
myAttrMap = attrMap V.defAttr
  [ (resultToAttr Correct,   bg V.green)
  , (resultToAttr Misplaced, bg V.yellow)
  , (resultToAttr Excluded,  bg V.brightBlack)
  ]

drawUI :: AppState -> [Widget ()]
drawUI as = pure $ vBox
  [ drawTitle as
  , drawWordle as
  , drawKeyboard as
  , drawDebug as
  ]

drawTitle AppState{day = day} = C.hCenter $ str $ "Wordle " ++ show day

drawWordle :: AppState -> Widget ()
drawWordle as@AppState{guesses = guesses, word = word}
  = C.center $ W.withBorderStyle BS.unicodeRounded $ B.border
    $ W.setAvailableSize (wordLength, numGuess) $ W.padBottom Max
    $ vBox $ reverse $ drawEntry as:map (drawGuess word) guesses

drawGuess :: String -> String -> Widget ()
drawGuess word guess = hBox $ map drawLetter $ wordleComp guess word

drawEntry :: AppState -> Widget ()
drawEntry AppState{entry = entry} = W.hLimit wordLength $ padRight Max $ str entry

drawDebug AppState{word = word, debug = debug}
  | debug     = str word
  | otherwise = str ""

drawKeyboard AppState{guesses = guesses, word = word} =
  vBox $ map C.hCenter keyboardRes
  where keyboard = ["qwertyuiop", "asdfghjkl", "zxcvbnm"]
        guessRes = concatMap (`wordleComp` word) guesses
        letterStatus c = fromMaybe Unknown $ maximumMay (guessRes `withKey` c)
        keyboardRes = map (hBox . map drawLetter . fmapTup letterStatus) keyboard

drawLetter :: (Char, Result) -> Widget ()
drawLetter (c, res) = withAttr (resultToAttr res) $ str [c]

