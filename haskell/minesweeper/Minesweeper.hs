module Minesweeper (annotate) where

import Data.List
import Data.Function
import Data.Char

type MineMatrix = [String]
type Index = (Int, Int)

annotate :: [String] -> [String]
annotate m = map (map (countMines m)) indexes
    where indexes    = groupBy ((==) `on` fst) [(x,y) | x <- [1..numRows], y <- [1..numColumns]]
          (numRows, numColumns) = getDimensions m
        

countMines :: MineMatrix -> Index -> Char
countMines m i | getValue m i == '*' = '*'
               | otherwise           = printableCount
    where printableCount = case getCount m i of 
                                0 -> ' '
                                n -> intToDigit n

getCount :: MineMatrix -> Index -> Int
getCount m i = length . filter (=='*') . map (getValue m) $ neighbors
    where neighbors = filter (withinBounds m) . map (`pairSum` i) $ deltas
          deltas = [(x,y) | x <- [-1..1], y <- [-1..1], (x,y) /= (0,0)]

withinBounds :: MineMatrix -> Index -> Bool
withinBounds m (x,y) = and [x > 0, x <= numRows, y > 0, y <= numColumns]
    where (numRows, numColumns) = getDimensions m

getValue :: MineMatrix -> Index -> Char
getValue m (x,y) = (m !! (x-1)) !! (y-1)

getDimensions :: MineMatrix -> (Int, Int)
getDimensions m = (numRows, numColumns)
    where numRows    = length m
          numColumns = case m of
                            []     -> 0
                            (x:xs) -> length x

pairSum :: (Int, Int) -> (Int, Int) -> (Int, Int)
pairSum (x,y) (z,w) = (x+z, y+w)

