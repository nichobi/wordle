module Util where

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = h : chunksOf n t
  where (h, t) = splitAt n xs

withKey :: eq a => [(a, b)] -> a -> [b]
withKey ((a,b):xs) y
  | a == y    = b : withKey xs y
  | otherwise = withKey xs y
withKey [] _ = []

fmapTup :: (a -> b) -> [a] -> [(a,b)]
fmapTup f x = zip x (map f x)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay xs = Just $ maximum xs

