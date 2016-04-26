module Grains(square, total) where

boardSquares :: Integer
boardSquares = 64

board :: [Integer]
board = map (\x -> 2^x) [0,1..]

square :: Int -> Integer
square n = board !! (n-1)

total :: Integer
total = sum $ take 64 board
