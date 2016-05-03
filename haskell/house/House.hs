module House(rhyme) where

import Data.List

beginning = "This is the "

middlePrefix = "that "

pairs = [("house that Jack built.", ""),
         ("malt", "lay in"),
         ("rat", "ate"),
         ("cat", "killed"),
         ("dog", "worried"),
         ("cow with the crumpled horn", "tossed"),
         ("maiden all forlorn", "milked"),
         ("man all tattered and torn", "kissed"),
         ("priest all shaven and shorn", "married"),
         ("rooster that crowed in the morn", "woke"),
         ("farmer sowing his corn", "kept"),
         ("horse and the hound and the horn", "belonged to")]

noun :: Int -> String
noun = fst . (pairs !!) . subtract 1

verb :: Int -> String
verb = snd . (pairs !!) . subtract 1

rhyme :: String
rhyme = concatMap ((++"\n") . rhymeForLength) [1..12]


rhymeForLength :: Int -> String
rhymeForLength n = unlines . map (rhymeLine n) . reverse $[1..n]

rhymeLine :: Int -> Int -> String
rhymeLine t p | t == p    = beginning ++ noun t
              | otherwise = middlePrefix ++ verb (p+1) ++ " the " ++ noun p

