module PrimeFactors where

import Data.List as List
import Data.Maybe

primeFactors :: Int -> [Int]
primeFactors n | n < 2     = []
               | otherwise = d:(primeFactors $ n `div` d)
    where d = fromJust $ List.find (\m -> n `mod` m == 0) [2,3..]
