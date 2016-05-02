module Clock (fromHourMin, toString) where

import Text.Printf

newtype Clock = Clock Integer deriving (Eq,Show)

instance Num Clock where
    Clock m + Clock n     = Clock . modDay $ m+n
    Clock m - Clock n     = Clock . modDay $ m-n
    Clock m * Clock n     = Clock . modDay $ m*n
    fromInteger n         = Clock . modDay $ n
    abs                   = error "Not implemented"
    signum                = error "Not implemented"
    
toString :: Clock -> String
toString (Clock n) = printf "%02d:%02d" hours minutes
    where (hours, minutes) = n `quotRem` 60

fromHourMin :: Integer -> Integer -> Clock
fromHourMin hours minutes = Clock . modDay $ 60*hours + minutes

modDay :: Integer -> Integer
modDay = (`mod` minutesInDay)
    where minutesInDay = 24*60
