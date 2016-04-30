module Atbash where

import Data.Maybe
import Data.Char
import Data.List.Split
import Data.List
import Data.Map(Map)
import qualified Data.Map as Map

alphabet = "abcdefghijklmnopqrstuvwxyz"

cipherMap = Map.fromList $ zip alphabet (reverse alphabet)

encode :: String -> String
encode = groupByFives . substitute . map toLower. filter isValidChar
    where substitute   = map (\c -> fromMaybe c $ Map.lookup c cipherMap)
          groupByFives = concat . intersperse " " . chunksOf 5
          isValidChar = (\x -> isAsciiUpper x ||  isAsciiLower x || isNumber x)
