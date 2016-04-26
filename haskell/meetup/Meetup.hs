module Meetup where

import qualified Data.Time.Calendar          as Calendar
import qualified Data.Time.Calendar.WeekDate as Weekdate

data Weekday = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday deriving (Show, Enum)

data Schedule = First | Second | Third | Fourth | Last | Teenth deriving (Show, Enum)

type Year = Integer
type Month = Int

meetupDay :: Schedule -> Weekday -> Year -> Month -> Calendar.Day
meetupDay Teenth day year month = Calendar.addDays diff firstOfMonth
    where diff = fromIntegral . last . takeWhile (<19) $ map (\x -> 7*x + diffToDayOfWeek) [1,2..]
          firstOfMonth    = Calendar.fromGregorian year month 1
          diffToDayOfWeek = fromIntegral $ forwardToDayOfWeek day firstOfMonth

meetupDay Last day year month = Calendar.addDays maxDiff firstOfMonth
    where maxDiff = fromIntegral . last . takeWhile (<monthDays) $ map (\x -> 7*x + diffToDayOfWeek) [1,2..]
          monthDays       = Calendar.gregorianMonthLength year month
          firstOfMonth    = Calendar.fromGregorian year month 1
          diffToDayOfWeek = fromIntegral $ forwardToDayOfWeek day firstOfMonth

meetupDay week day year month = Calendar.addDays (diffToDayOfWeek + weekGap) firstOfMonth
    where   firstOfMonth    = Calendar.fromGregorian year month 1
            diffToDayOfWeek = fromIntegral $ forwardToDayOfWeek day firstOfMonth
            weekGap         = (fromIntegral . (*7) . fromEnum $  week)

forwardToDayOfWeek :: Weekday -> Calendar.Day -> Int
forwardToDayOfWeek targetWeekday firstOfMonth = if diffToDayOfWeek >= 0 
                                                   then diffToDayOfWeek 
                                                   else diffToDayOfWeek + 7
    where
        firstDayOfWeek     = fromIntegral . third . Weekdate.toWeekDate $ firstOfMonth
        diffToDayOfWeek    = (fromIntegral $ fromEnum targetWeekday) - firstDayOfWeek


third :: (a,b,c) -> c
third (x,y,z) = z
