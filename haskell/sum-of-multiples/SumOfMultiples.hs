module SumOfMultiples where

import Data.List

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples [] n = sumOfMultiples [3,5] n
sumOfMultiples xs n = sum $ foldl union [] (map (findMultiples n) xs )
    where findMultiples n m = takeWhile (<n) $ map (*m) [1,2..]

