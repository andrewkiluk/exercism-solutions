module Anagram(anagramsFor) where

import Data.List
import Data.Char

anagramsFor :: String -> [String] -> [String]
anagramsFor base candidates = filter (\x -> (map toLower x) `elem` loweredPermutations) candidates
    where loweredPermutations = filter (/=loweredBase) $ permutations loweredBase
          loweredBase = map toLower base

