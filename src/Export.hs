module Export (export) where

import Util
import WordleComp

export :: AppState -> String
export as = exportText as ++ "\n\n" ++ exportGrid as

exportText :: AppState -> String
exportText AppState{guesses = guesses, day = day, status = status} =
  concat ["Wordle ", show day, " ", score, "/", show numGuess]
  where score = case status of
                  Won        -> show $ length guesses
                  Lost       -> "X"
                  InProgress -> error "Exporting in progress game"

exportGrid :: AppState -> String
exportGrid AppState{word = word, guesses = guesses}
  = unlines $ reverse $ map (map (resultToEmoji . snd) . (`wordleComp` word)) guesses

resultToEmoji :: Result -> Char
resultToEmoji r = case r of
  Excluded  -> '⬛'
  Misplaced -> '🟨'
  Correct   -> '🟩'
  Unknown   -> error "Unknown result in export"

