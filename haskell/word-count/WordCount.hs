module WordCount where

import Data.Char
import Data.Map(Map)
import qualified Data.Map as Map

wordCount :: String -> Map String Int
wordCount = buildMap . words . (map toLower) . (map (\c -> if isAlphaNum c then c else ' '))
    where buildMap = foldr (\w m -> Map.insertWith (+) w 1 m) Map.empty
