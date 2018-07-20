--- ----------------------------------------------------------------------------
--- This module defines some utility functions for the partial evaluator.
---
--- @author  Björn Peemöller
--- @version September 2015
--- ----------------------------------------------------------------------------
module Utils where

import Data.List (intercalate, intersect)

--- Does no element satisfy the predicate?
none :: (a -> Bool) -> [a] -> Bool
none p xs = not (any p xs)

--- Do two lists have the same length?
sameLength :: [a] -> [b] -> Bool
sameLength xs ys = length xs == length ys

--- Count the number of occurrences of an element in a list.
count :: Eq a => a -> [a] -> Int
count _ []                 = 0
count x (y:ys) | x == y    = 1 + count x ys
               | otherwise = count x ys

--- Count the number of elements in a list that satisfy the given predicate.
countBy :: (a -> Bool) -> [a] -> Int
countBy p xs = length (filter p xs)

--- Check if the two lists are disjoint.
disjoint :: Eq a => [a] -> [a] -> Bool
disjoint xs ys = null (intersect xs ys)

--- Drop the last `n` elements from a given list.
dropLast :: Int -> [a] -> [a]
dropLast n xs = reverse (drop n (reverse xs))

--- Pad a string to a specific length with space from the left side.
lpad :: Int -> String -> String
lpad n s = replicate (n - length s) ' ' ++ s

--- Pad a string to a specific length with space from the right side.
rpad :: Int -> String -> String
rpad n str = str ++ replicate (n - length str) ' '

--- Indent a `String`, possibly containing multiple lines via newline,
--- by a given number of spaces for each single line.
indentStr :: Int -> String -> String
indentStr n str = intercalate "\n" $ map (replicate n ' ' ++) $ lines str

--- `String` concatenation with newline in between.
(+\+) :: String -> String -> String
s1 +\+ s2 = s1 ++ '\n' : s2
