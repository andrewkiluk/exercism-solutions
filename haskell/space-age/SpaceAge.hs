module SpaceAge(Planet(..), ageOn) where

import Data.Maybe
import Data.Map(Map)
import qualified Data.Map as Map

data Planet = Mercury | Venus | Earth | Mars | Jupiter | Saturn | Uranus | Neptune deriving (Eq, Ord)
type Seconds = Integer
type Years = Double

secondsInEarthYear = 31557600

planetRatios = Map.fromList [ (Mercury, 0.2408467),
 (Venus, 0.61519726),
 (Earth, 1),
 (Mars, 1.8808158),
 (Jupiter, 11.862615),
 (Saturn, 29.447498),
 (Uranus, 84.016846),
 (Neptune, 164.79132)]

ageOn :: Planet -> Seconds -> Years
ageOn planet s = (fromIntegral s) / planetYearSeconds
    where planetYearSeconds = secondsInEarthYear * (fromMaybe 1 (Map.lookup planet planetRatios))
