module Util where

-- Constants

wordLength = 5 :: Int
numGuess   = 6 :: Int

scoreMessage :: Int -> String
scoreMessage x = case x of
  1 -> "Genius"
  2 -> "Magnificent"
  3 -> "Impressive"
  4 -> "Splendid"
  5 -> "Great"
  6 -> "Phew"

-- Datatypes

data Result = Unknown | Excluded | Misplaced | Correct
  deriving (Show, Eq, Ord)
data Status = InProgress | Won | Lost
  deriving (Show, Eq, Ord)

data AppState = AppState
  { word       :: String
  , status     :: Status
  , guesses    :: [String]
  , entry      :: String
  , day        :: Int
  , dictionary :: [String]
  , message    :: String
  , debug      :: Bool
  }

-- Functions

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = h : chunksOf n t
  where (h, t) = splitAt n xs

withKey :: Eq a => [(a, b)] -> a -> [b]
withKey ((a,b):xs) y
  | a == y    = b : withKey xs y
  | otherwise = withKey xs y
withKey [] _ = []

fmapTup :: (a -> b) -> [a] -> [(a,b)]
fmapTup f x = zip x (map f x)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay xs = Just $ maximum xs

