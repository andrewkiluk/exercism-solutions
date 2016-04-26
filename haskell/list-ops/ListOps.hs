module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc []     = acc
foldl' f acc (x:xs) = seq acc' $ foldl' f acc' xs
    where acc' = acc `f` x

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ acc []     = acc
foldr f acc (x:xs) = x `f` foldr f acc xs

length :: [a] -> Int
length = foldr (\_ acc -> acc+1) 0

reverse :: [a] -> [a]
reverse = foldr (\x acc -> acc ++ [x]) []

map :: (a -> b) -> [a] -> [b]
map f xs = foldr (\x acc -> f x : acc) [] xs

filter :: (a -> Bool) -> [a] -> [a]
filter f xs = foldr (\x acc -> if (f x) then (x:acc) else acc) [] xs

(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (\x acc -> x:acc) ys xs

concat :: [[a]] -> [a]
concat = foldr (++) [] 
