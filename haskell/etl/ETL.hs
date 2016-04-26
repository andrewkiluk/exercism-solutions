module ETL where

import Data.Char
import Data.Map(Map)
import qualified Data.Map as Map

transform :: Map Int [String] -> Map String Int
transform = Map.unions . (map Map.fromList) . (map convertValueToNewFormat) . Map.toList

convertValueToNewFormat :: (Int, [String]) -> [(String, Int)]
convertValueToNewFormat (value, tiles) = map (\tile -> (map toLower tile, value)) tiles
