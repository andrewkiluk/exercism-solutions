module Gigasecond(fromDay) where

import Data.Time.Clock

gigasecond = fromInteger 10^9 :: NominalDiffTime

fromDay :: UTCTime -> UTCTime
fromDay date = addUTCTime gigasecond date
