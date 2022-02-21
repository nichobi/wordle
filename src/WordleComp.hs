module WordleComp (wordleComp) where

import Util

import Data.List (delete)

-- Takes a guess and a target word, returns a list of guessed letters paired
-- with their match result
wordleComp :: String -> String -> [(Char, Result)]
wordleComp guess word = map (\(x,_,r) -> (x, r)) $ findMisplaced $ findCorrect guess word

-- Initial sweep of letters, marking only correct or excluded
findCorrect :: String -> String -> [(Char, Char, Result)]
findCorrect = zipWith f
  where f :: Char -> Char -> (Char, Char, Result)
        f x y
          | x == y    = (x, y, Correct)
          | otherwise = (x, y, Excluded)

-- Second sweep, remarking any misplaced letters
findMisplaced :: [(Char, Char, Result)] -> [(Char, Char, Result)]
findMisplaced xs = helper xs (unmatchedYs xs)
  where helper [] _ = []
        helper ((x, y, Excluded):xs) unmatched
          | x `elem` unmatched  = (x, y, Misplaced) : helper xs (x `delete` unmatched)
          | otherwise           = (x, y, Excluded)  : helper xs unmatched
        helper (x:xs) unmatched = x : helper xs unmatched
        unmatchedYs ((_, y, Excluded):xs) = y : unmatchedYs xs
        unmatchedYs (_:xs)                = unmatchedYs xs
        unmatchedYs [] = []

