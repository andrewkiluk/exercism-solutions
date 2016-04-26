module Beer(sing, verse) where

import Text.Printf

sing :: Int -> Int -> String
sing start stop = concat . map (++"\n") .reverse . drop stop . take (start+1) $ verses
 
verse :: Int -> String
verse = (verses !!)

verses :: [String]
verses = map makeVerse [0,1..]


normalVerse = "%s bottle%s of beer on the wall, %s bottle%s of beer.\n\
\Take %s down and pass it around, %s bottle%s of beer on the wall.\n"
emptyVerse = "No more bottles of beer on the wall, no more bottles of beer.\n\
\Go to the store and buy some more, 99 bottles of beer on the wall.\n"

makeVerse :: Int -> String
makeVerse 0 = emptyVerse
makeVerse 1 = printf normalVerse "1" "" "1" "" "it" "no more" "s"
makeVerse 2 = printf normalVerse "2" "s" "2" "s" "one" "1" ""
makeVerse n = printf normalVerse (show n) "s" (show n) "s" "one" (show $ n-1) "s"

    
