module Raindrops(convert) where

import Data.Maybe

convert :: Int -> String
convert n = if null $ rainSounds n 
               then show n 
               else concat $ rainSounds n

rainSounds :: Int -> [String]
rainSounds n = catMaybes [pling n, plang n, plong n]
    where pling m = if m `mod` 3 == 0 then Just "Pling" else Nothing
          plang m = if m `mod` 5 == 0 then Just "Plang" else Nothing
          plong m = if m `mod` 7 == 0 then Just "Plong" else Nothing
