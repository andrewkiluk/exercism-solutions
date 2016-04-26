module Strain where

keep p xs = fst $ splitOn p xs
discard p xs = snd $ splitOn p xs

splitOn :: (a -> Bool) -> [a] -> ([a],[a])
splitOn p xs = foldl (classify p) ([],[]) xs
    where classify p (yes, no) x | p x       = (yes ++ [x], no)
                                 | otherwise = (yes, no ++ [x])

