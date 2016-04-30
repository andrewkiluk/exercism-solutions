module Garden (garden, defaultGarden, lookupPlants, Plant(..)) where

import Data.Maybe
import Data.List
import Data.List.Split
import Data.Map(Map)
import qualified Data.Map as Map

data Plant = Clover | Grass | Radishes | Violets deriving (Show, Eq)

type Name = String
type Garden = Map Name [Plant]

defaultStudents = ["Alice", "Bob", "Charlie", "David", "Eve", "Fred", "Ginny", 
    "Harriet", "Ileana", "Joseph", "Kincaid", "Larry"] :: [Name]

lookupPlants :: Name -> Garden -> [Plant]
lookupPlants n g = fromMaybe [] $ Map.lookup n g 

defaultGarden :: String -> Garden
defaultGarden = garden defaultStudents

garden :: [Name] -> String -> Garden
garden names input = Map.fromList plantsByStudent
    where chunked         = map (chunksOf 2) . lines $ input
          inputByFours    = zipWith (++) (chunked !! 0) (chunked !! 1)
          plantsByFours   = map (map charToPlant) inputByFours
          plantsByStudent = zip (sort names) plantsByFours
          
charToPlant :: Char -> Plant
charToPlant c | c == 'C' = Clover
              | c == 'G' = Grass
              | c == 'R' = Radishes
              | c == 'V' = Violets
