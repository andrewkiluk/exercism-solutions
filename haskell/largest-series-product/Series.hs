module Series(largestProduct) where

import Data.List
import Data.Char

largestProduct :: Int -> String -> Maybe Int
largestProduct len input | length input < len = Nothing
                         | otherwise          = Just . maximum . findProducts len $ input

findProducts :: Int -> String -> [Int]
findProducts _ [] = [1]
findProducts n s  = map product . windows n . map digitToInt $ s

windows :: Int -> [a] -> [[a]]
windows _ []               = []
windows k l | length l < k = []
            | otherwise    = take k l:windows k (tail l)
