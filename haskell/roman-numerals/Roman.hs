module Roman where

numerals :: Int -> String
numerals x | x >= 1000 = (calculateThousands x) ++ numerals (x `mod` 1000)
numerals x | x >= 100 = (calculateHundreds x) ++ numerals (x `mod` 100)
numerals x | x >= 10 = (calculateTens x) ++ numerals (x `mod` 10)
numerals x = calculateOnes x

calculateThousands = calculate 1000 "M" "" ""
calculateHundreds = calculate 100 "C" "D" "M"
calculateTens = calculate 10 "X" "L" "C"
calculateOnes = calculate 1 "I" "V" "X"

calculate magnitude low med high x | x >= 9*magnitude = low ++ high
                                   | x >= 8*magnitude = med ++ low ++ low ++ low
                                   | x >= 7*magnitude = med ++ low ++ low
                                   | x >= 6*magnitude = med ++ low
                                   | x >= 5*magnitude = med
                                   | x >= 4*magnitude = low ++ med
                                   | x >= 3*magnitude = low ++ low ++ low
                                   | x >= 2*magnitude = low ++ low
                                   | x >= 1*magnitude = low
                                   | otherwise = ""
