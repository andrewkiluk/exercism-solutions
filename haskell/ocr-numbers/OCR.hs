module OCR (
  convert,
) where

import Data.List.Extra

convert :: String -> String
convert = intercalate "," . map (map convertBlock) . convertToBlocks . lines

convertToBlocks :: [String] -> [[[String]]]
convertToBlocks = map transpose . chunksOf 4 . map (chunksOf 3)


convertBlock :: [String] -> Char
convertBlock x | x == zero  = '0'
               | x == one   = '1'
               | x == two   = '2'
               | x == three = '3'
               | x == four  = '4'
               | x == five  = '5'
               | x == six   = '6'
               | x == seven = '7'
               | x == eight = '8'
               | x == nine  = '9'
               | otherwise  = '?'
    where zero =  [" _ ",
                   "| |",
                   "|_|",
                   "   "]
          one =   ["   ",
                   "  |",
                   "  |",
                   "   "]
          two =   [" _ ",
                   " _|",
                   "|_ ",
                   "   "]
          three = [" _ ",
                   " _|",
                   " _|",
                   "   "]
          four =  ["   ",
                   "|_|",
                   "  |",
                   "   "]
          five =  [" _ ",
                   "|_ ",
                   " _|",
                   "   "]
          six =   [" _ ",
                   "|_ ",
                   "|_|",
                   "   "]
          seven = [" _ ",
                   "  |",
                   "  |",
                   "   "]
          eight = [" _ ",
                   "|_|",
                   "|_|",
                   "   "]
          nine =  [" _ ",
                   "|_|",
                   " _|",
                   "   "]

