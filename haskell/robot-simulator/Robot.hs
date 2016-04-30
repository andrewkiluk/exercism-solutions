module Robot (Bearing(..), Robot, mkRobot,
              coordinates, simulate,
              bearing, turnRight, turnLeft)
    where

data Bearing = East | North | West | South deriving (Show, Eq, Enum)

type Position = (Int, Int)

data Robot = Robot { bearing :: Bearing, coordinates :: Position } deriving (Eq, Show)

mkRobot :: Bearing -> Position -> Robot
mkRobot b c = Robot{bearing = b, coordinates=c}

turnRight :: Bearing -> Bearing
turnRight = toEnum . (`mod` 4) . subtract 1 . fromEnum

turnLeft :: Bearing -> Bearing
turnLeft = toEnum . (`mod` 4) . (+1) . fromEnum

advance :: Robot -> Robot
advance robot@(Robot b (x,y)) | b == East  = robot{coordinates=(x+1,y)}
                              | b == North = robot{coordinates=(x,y+1)}
                              | b == West = robot{coordinates=(x-1,y)}
                              | b == South = robot{coordinates=(x,y-1)}

simulate :: Robot -> String -> Robot
simulate = foldl instruct
    where instruct robot c | c == 'L' = robot{bearing = turnLeft $ bearing robot}
                           | c == 'R' = robot{bearing = turnRight $ bearing robot}
                           | c == 'A' = advance robot

