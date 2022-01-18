module Main where

import Paths_wordle
import Util

import Data.Char    (isAlpha, isLower)
import Data.List    (delete)
import Data.Maybe   (fromMaybe)
import Data.Time    (diffDays, fromGregorian, getZonedTime, localDay, zonedTimeToLocalTime)
import Graphics.Vty (Key (..), Event (..))

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

data Result = Unknown | Excluded | Misplaced | Correct
  deriving (Show, Eq, Ord)
data Status = InProgress | Won | Lost

resultToAttr :: Result -> AttrName
resultToAttr = attrName . show

wordLength = 5
numGuess = 6

data AppState = AppState
  { word       :: String
  , status     :: Status
  , guesses    :: [String]
  , entry      :: String
  , day        :: Int
  , dictionary :: [String]
  , debug      :: Bool
  }

initState word dictionary day = AppState
  { word       = word
  , status     = InProgress
  , guesses    = []
  , entry      = []
  , day        = day
  , dictionary = dictionary
  , debug      = False
  }

main = do
  dayNumber  <- getDayNumber
  word       <- readWordle dayNumber
  dictionary <- getDictionary
  defaultMain app (initState word dictionary dayNumber)

getDayNumber :: IO Int
getDayNumber = do
  today <- localDay . zonedTimeToLocalTime <$> getZonedTime
  let dayZero = fromGregorian 2021 06 19
  pure $ fromInteger $ diffDays today dayZero


readWordle :: Int -> IO String
readWordle dayNumber = do
  file <- getDataFileName "data/wordlesraw.txt"
  take wordLength . drop (dayNumber * wordLength) <$> readFile file

getDictionary :: IO [String]
getDictionary = do
  dictFile      <- getDataFileName "data/dictionaryraw.txt"
  wordlesFile   <- getDataFileName "data/wordlesraw.txt"
  rawDictionary <- init <$> readFile dictFile
  wordles       <- init <$> readFile wordlesFile
  pure $ chunksOf wordLength (wordles ++ rawDictionary)

app = App { appDraw         = drawUI
          , appHandleEvent  = handleEvent
          , appStartEvent   = return
          , appAttrMap      = const myAttrMap
          , appChooseCursor = showFirstCursor
          }

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

drawTitle AppState{day = day} = C.hCenter $ str $ "Wordle " ++ (show day)

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

handleEvent :: AppState -> BrickEvent () e -> EventM () (Next AppState)
handleEvent as@AppState{status = InProgress} e = case e of
  (VtyEvent (EvKey (KChar c) []))         -> (continue . insertChar c) as
  (VtyEvent (EvKey KBS       []))         -> (continue . removeChar)   as
  (VtyEvent (EvKey KEnter    []))         -> (continue . makeGuess)    as
--(VtyEvent (EvKey KEsc      []))         -> (continue . toggleDebug)  as
  (VtyEvent (EvKey (KChar c)  [V.MCtrl])) -> halt                      as
  _                                       -> continueWithoutRedraw     as
handleEvent as e = case e of
  (VtyEvent (EvKey (KChar c)  [V.MCtrl])) -> halt                      as
  _                                       -> continueWithoutRedraw     as

makeGuess :: AppState -> AppState
makeGuess as@AppState{entry=entry, word=word, guesses=guesses, dictionary = dict}
  | word == entry = as{entry = [], guesses = entry : guesses, status = Won}
  | length entry == wordLength && entry `elem` dict
     = as{ entry = []
         , guesses = entry : guesses
         , status = if length guesses == numGuess then Lost else InProgress
         }
  | otherwise     = as

insertChar :: Char -> AppState -> AppState
insertChar c as@AppState{entry = entry}
  | length entry < wordLength && isLower c = as{entry = entry ++ [c]}
  | otherwise                              = as

removeChar :: AppState -> AppState
removeChar as@AppState{entry = entry}
  | null entry = as
  | otherwise  = as{entry = init entry}

toggleDebug :: AppState -> AppState
toggleDebug as@AppState{debug = debug} = as{debug = not debug}

wordleComp :: String -> String -> [(Char, Result)]
wordleComp guess word = map (\(x,_,r) -> (x, r)) $ findMisplaced $ findCorrect guess word
  where findCorrect [] [] = []
        findCorrect (x:xs) (y:ys)
          | x == y        = (x, y, Correct)  : continue
          | otherwise     = (x, y, Excluded) : continue
          where continue = findCorrect xs ys
        findMisplaced xs = helper xs (unmatchedYs xs)
          where helper [] _ = []
                helper ((x, y, Excluded):xs) unmatched
                  | x `elem` unmatched  = (x, y, Misplaced): helper xs (x `delete` unmatched)
                  | otherwise           = (x, y, Excluded):  helper xs unmatched
                helper (x:xs) unmatched = x:helper xs unmatched
        unmatchedYs ((_, y, Excluded):xs) = y:unmatchedYs xs
        unmatchedYs (_:xs)                = unmatchedYs xs
        unmatchedYs [] = []

