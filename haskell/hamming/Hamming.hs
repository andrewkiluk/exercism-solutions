module Hamming where

distance :: String -> String -> Int
distance xs ys = sum . (map fromEnum) $ (zipWith (/=) xs ys)
