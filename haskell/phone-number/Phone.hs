module Phone(areaCode, number, prettyPrint) where

import Data.Char
import Text.Printf

areaCode :: String -> String
areaCode = take 3 . number

number :: String -> String
number input 
    | length parsed == 10                            = parsed
    | (length parsed == 11) && (head parsed == '1')  = tail parsed
    | otherwise                                      = "0000000000"
    where parsed = filter isNumber input

prettyPrint :: String -> String
prettyPrint input = printf "(%s) %s-%s" area exchange local
    where fullNumber = number input
          area       = areaCode fullNumber
          exchange   = take 3 $ drop 3 $ fullNumber
          local      = take 4 $ drop 6 $ fullNumber

