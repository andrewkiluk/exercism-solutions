module School(School, add, sorted, grade, empty) where

import Data.List
import Data.Ord
import Data.Maybe

type Grade = Int
type Student = String
type School = [(Grade, [Student])]

empty = []

add :: Grade -> Student -> School -> School
add g student school = (g, newRoster) : schoolMinusGrade
    where schoolMinusGrade = filter ((g /=) . fst) school
          newRoster = student : grade g school

sorted :: School -> School
sorted school = sortBy (comparing fst) school'
    where school' = map (\(g,s) -> (g, sort s)) school

grade :: Grade -> School -> [Student]
grade g s = (fromMaybe []) $ lookup g s
