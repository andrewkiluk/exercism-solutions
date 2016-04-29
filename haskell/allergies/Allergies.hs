module Allergies where

import Data.Maybe

data Allergen = Eggs | Peanuts | Shellfish | Strawberries | Tomatoes | Chocolate | Pollen | Cats deriving (Show, Enum, Bounded, Eq)

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo a score = a `elem` (allergies score)

allergies :: Int -> [Allergen]
allergies score = selectActive $ zip (reverse $ toBinary score) allAllergens
    where allAllergens = [(minBound :: Allergen)..]
          selectActive = map snd . filter ((=='1') . fst)

toBinary :: Int -> String
toBinary n | n < 2     = show n
           | otherwise = toBinary q ++ toBinary r
    where (q,r) = n `quotRem` 2
