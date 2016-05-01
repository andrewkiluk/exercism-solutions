module Queens (boardString, canAttack) where

type Position = (Int, Int)

boardSize :: Int
boardSize = 8 

emptyChar :: Char
emptyChar = '_'

emptyLine :: String
emptyLine = (++"\n") . take (2*boardSize-1) . concat $ repeat [emptyChar, ' ']

emptyBoardLines :: [String]
emptyBoardLines = replicate boardSize emptyLine

emptyBoard :: String
emptyBoard = concat emptyBoardLines


canAttack :: Position -> Position -> Bool
canAttack target (x,y) | (x,y) == target   = True
                       | outOfBounds (x,y) = False
                       | otherwise         = any (hitsTarget target (x,y)) directions
    where directions = [(a,b) | a<-[-1,0,1], b<-[-1,0,1], (a,b) /= (0,0)]

boardString :: Maybe Position -> Maybe Position -> String
boardString Nothing Nothing = emptyBoard
boardString (Just p) Nothing = overlay (boardWithQueen 'W' p) emptyBoard
boardString Nothing (Just p) = overlay emptyBoard (boardWithQueen 'B' p) 
boardString (Just p) (Just q) = overlay (boardWithQueen 'W' p) (boardWithQueen 'B' q) 

-- Minions

hitsTarget :: Position -> Position -> (Int, Int) -> Bool
hitsTarget target (x,y) (v,w) | (x,y) == target   = True
                              | outOfBounds (x,y) = False
                              | otherwise         = hitsTarget target (x+v,y+w) (v,w)

outOfBounds :: Position -> Bool
outOfBounds (x,y) = x<0 || x>=8 || y<0 || y>=8

boardWithQueen :: Char -> Position -> String
boardWithQueen color (x,y) = concat $ setAt x queenRow emptyBoardLines
    where queenRow = setAt (2*y) color emptyLine

setAt :: Int -> a -> [a] -> [a]
setAt p value list = before ++ value:tail after
    where (before, after) = splitAt p list

overlay :: String -> String -> String
overlay = zipWith firstNotEmpty
    where firstNotEmpty a b | a /= emptyChar = a
                            | b /= emptyChar = b
                            | otherwise      = emptyChar
