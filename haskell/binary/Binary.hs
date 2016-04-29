module Binary where

import Data.Char

toDecimal :: String -> Int
toDecimal s | all isDigit s = sum $ zipWith (*) reversedBits powersOfTwo
            | otherwise     = 0
    where reversedBits = reverse (map digitToInt s)
          powersOfTwo  = zipWith (^) (repeat 2) [0,1..]

