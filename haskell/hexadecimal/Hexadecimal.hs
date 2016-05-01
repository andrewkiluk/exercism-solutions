module Hexadecimal (hexToInt) where

import Data.Maybe
import Data.Map(Map)
import qualified Data.Map as Map

hexToInt :: String -> Int
hexToInt hex = if all isValidHex hex
                  then sum $ zipWith (*) (reverse $ map toDecValue hex) powersOf16
                  else 0
    where powersOf16 = map (16^) [0,1..]

isValidHex :: Char -> Bool
isValidHex = (`elem` Map.keys hexToDec)

toDecValue :: Char -> Int
toDecValue = fromMaybe 0 . (`Map.lookup` hexToDec) 

hexToDec :: Map Char Int
hexToDec = Map.fromList [('0',0),
                         ('1',1),
                         ('2',2),
                         ('3',3),
                         ('4',4),
                         ('5',5),
                         ('6',6),
                         ('7',7),
                         ('8',8),
                         ('9',9),
                         ('a',10),
                         ('b',11),
                         ('c',12),
                         ('d',13),
                         ('e',14),
                         ('f',15)]


