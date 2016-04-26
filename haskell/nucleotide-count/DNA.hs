module DNA(count, nucleotideCounts) where

import Data.List
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Maybe
import Text.Printf

type Base = Char

count :: Base -> String -> Int
count base strand
    | isValidBase base = fromMaybe 0 $ Map.lookup base (nucleotideCounts strand)
    | otherwise = invalidBase base

nucleotideCounts:: String -> Map Base Int
nucleotideCounts strand 
    | all isValidBase strand = foldl countBase baseMap strand
    | otherwise                   = invalidBase (fromJust $ find (not . isValidBase) strand)
    where countBase m base = Map.insertWith (+) base 1 m
          baseMap = Map.fromList [('A',0),('C',0),('G',0),('T',0)]

isValidBase base = base `elem` ['A', 'C', 'G', 'T']

invalidBase base = error (printf "invalid nucleotide '%c'" base)
