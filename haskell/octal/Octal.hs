module Octal (showOct, readOct) where

import Data.Char

showOct :: (Integral a, Show a) => a -> String
showOct = reverse . showOct'
    where showOct' n | n < 8     = show n
                     | otherwise = intToDigit (fromIntegral rem) : showOct' quot
              where (quot, rem) = n `divMod` 8

readOct :: (Integral a) => String -> a
readOct = fromIntegral . sum . zipWith (*) powersOfEight . reverse . (map digitToInt)
    where powersOfEight = map (8^) [0,1..]
