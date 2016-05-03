module Matrix ( Matrix, row, column, rows, cols, shape, transpose
              , reshape, flatten, fromString, fromList) where

import Data.List
import Data.List.Split
import qualified Data.Vector as V

type Matrix a = [[a]]

row :: Int -> Matrix a -> V.Vector a
row n m = V.fromList $ m !! n

column :: Int -> Matrix a -> V.Vector a
column n m = row n . transpose $ m

rows :: Matrix a -> Int
rows [] = 0
rows m  = length . column 0 $ m

cols :: Matrix a -> Int
cols [] = 0
cols m  = length . row 0 $ m

shape :: Matrix a -> (Int, Int)
shape m = (rows m, cols m)

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (x,_) m = chunksOf x . V.toList $ flatten m

flatten :: Matrix a -> V.Vector a
flatten = V.fromList . concat

fromList :: [[a]] -> Matrix a
fromList = id

fromString :: (Read a) => String -> Matrix a
fromString = map (unfoldr $ listToMaybe . reads) . lines
