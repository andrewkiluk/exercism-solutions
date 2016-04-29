module Roman where

numerals :: Int -> String
numerals x | x >= 1000 = (calculateThousands x) ++ numerals (x `mod` 1000)
numerals x | x >= 100 = (calculateHundreds x) ++ numerals (x `mod` 100)
numerals x | x >= 10 = (calculateTens x) ++ numerals (x `mod` 10)
numerals x = calculateOnes x

calculateThousands x = if x >= 1000 then "M" ++ calculateThousands (x - 1000) else ""

calculateHundreds x | x >= 900  = "CM"
                    | x >= 800  = "DCCC"
                    | x >= 700  = "DCC"
                    | x >= 600  = "DC"
                    | x >= 500  = "D"
                    | x >= 400  = "CD"
                    | x >= 300  = "CCC"
                    | x >= 200  = "CC"
                    | x >= 100  = "C"
                    | otherwise = ""

calculateTens x | x >= 90   = "XC"
                | x >= 80   = "LXXX"
                | x >= 70   = "LXX"
                | x >= 60   = "LX"
                | x >= 50   = "L"
                | x >= 40   = "XL"
                | x >= 30   = "XXX"
                | x >= 20   = "XX"
                | x >= 10   = "X"
                | otherwise = ""

calculateOnes x | x >= 9    = "IX"
                | x >= 8    = "VIII"
                | x >= 7    = "VII"
                | x >= 6    = "VI"
                | x >= 5    = "V"
                | x >= 4    = "IV"
                | x >= 3    = "III"
                | x >= 2    = "II"
                | x >= 1    = "I"
                | otherwise = ""
