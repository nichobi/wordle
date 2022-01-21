module Main where

import Util
import Paths_wordle
import Draw (drawUI, myAttrMap)
import Export (export)

import Data.Char  (isLower)
import Data.Time  (diffDays, fromGregorian, getZonedTime, localDay, zonedTimeToLocalTime)
import Graphics.Vty (Key (..), Event (..))
import qualified Graphics.Vty as V

import Brick
  ( App(..), BrickEvent(..), EventM, Next
  , defaultMain, showFirstCursor
  , continue, halt, continueWithoutRedraw
  )
import Control.Monad (when)

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
  finalState <- defaultMain app (initState word dictionary dayNumber)
  when (status finalState `elem` [Won, Lost]) (putStrLn $ export finalState)

getDayNumber :: IO Int
getDayNumber = do
  today <- localDay . zonedTimeToLocalTime <$> getZonedTime
  pure $ fromInteger $ diffDays today dayZero
    where dayZero = fromGregorian 2021 06 19


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
     = as{ entry   = []
         , guesses = entry:guesses
         , status  = if length (entry:guesses) == numGuess then Lost else InProgress
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

