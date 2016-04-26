module Bob(responseFor) where 

import Data.Char

responseFor :: String -> String
responseFor = getResponse . stripWhiteSpace
    where stripWhiteSpace = filter (not . isSpace)

getResponse :: String -> String
getResponse s | all isSpace s = "Fine. Be that way!"
              | (any isAlpha s) && allCaps s = "Whoa, chill out!"
              | last s == '?' = "Sure."
              | otherwise = "Whatever."
    where allCaps = (all isUpper) . (filter isAlpha)
