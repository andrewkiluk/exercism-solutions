module Triangle where

import Data.List

data TriangleType = Equilateral | Isosceles | Scalene | Illogical deriving (Show, Eq)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType x y z | notTriangle a b c = Illogical
                   | (a==b) && (b==c)  = Equilateral
                   | (a==b) || (b==c)  = Isosceles
                   | otherwise         = Scalene
    where [a,b,c] = sort [x,y,z]

notTriangle a b c = (a+b<=c) || (b+c<=a) || (c+a<=b)
