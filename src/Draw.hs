module Draw (drawUI, myAttrMap) where

import Util
import WordleComp (wordleComp)

import Data.Maybe (fromMaybe)

import Brick
  ( AttrMap, Widget
  , str, vBox, hBox
  , padLeft, padTop, Padding (Max, Pad)
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
  , C.center $ hBox
    [ drawWordle as
    , padTop (Pad 2) $ padLeft (Pad 4) $ drawKeyboard as
    ]
  , drawMessage as
  , drawDebug as
  ]

drawTitle :: AppState -> Widget n
drawTitle AppState{day = day} = C.hCenter $ str $ "Wordle " ++ show day

drawWordle :: AppState -> Widget ()
drawWordle as@AppState{guesses = guesses, word = word}
  = W.withBorderStyle BS.unicodeRounded $ B.border
    $ W.setAvailableSize (wordLength, numGuess) $ W.padBottom Max
    $ vBox $ reverse $ drawEntry as:map (drawGuess word) guesses

drawGuess :: String -> String -> Widget ()
drawGuess word guess = hBox $ map drawLetter $ wordleComp guess word

drawEntry :: AppState -> Widget ()
drawEntry AppState{entry = entry} = str (entry ++ spaces)
  where spaces = replicate (wordLength - length entry) ' '

drawDebug :: AppState -> Widget n
drawDebug AppState{word = word, debug = debug}
  | debug     = str word
  | otherwise = W.emptyWidget

drawKeyboard :: AppState -> Widget ()
drawKeyboard AppState{guesses = guesses, word = word} =
  vBox keyboardRes
  where keyboard = ["qwertyuiop", "asdfghjkl", " zxcvbnm"]
        guessRes = concatMap (`wordleComp` word) guesses
        letterStatus c = fromMaybe Unknown $ maximumMay (guessRes `withKey` c)
        keyboardRes = map (hBox . map drawLetter . fmapTup letterStatus) keyboard

drawLetter :: (Char, Result) -> Widget ()
drawLetter (c, res) = withAttr (resultToAttr res) $ str [c]

drawMessage :: AppState -> Widget ()
drawMessage AppState{message = ""}      = C.hCenter $ str " "
drawMessage AppState{message = message} = C.hCenter $ str message

