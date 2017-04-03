module append4 ( append4.PEVAL, (append4.++), append4.goal, append4.main ) where

import Prelude

append4.PEVAL :: a -> a
append4.PEVAL v1 = v1

(append4.++) :: [a] -> [a] -> [a]
(append4.++) v1 v2 = fcase v1 of
    [] -> v2
    v3 : v4 -> v3 : (v4 append4.++ v2)

append4.goal :: [a] -> [a] -> [a] -> [a]
append4.goal v1 v2 v3 = append4._pe0 v1 v2 v3

append4.main :: [Prelude.Int]
append4.main = append4.goal
  (Prelude.enumFromTo 1 3)
  (Prelude.enumFromTo 4 6)
  (Prelude.enumFromTo 7 10)

append4._pe0 :: [a] -> [a] -> [a] -> [a]
append4._pe0 v1 v2 v3 = fcase v1 of
    [] -> fcase v2 of
        [] -> v3
        v4 : v5 -> v4 : (v5 append4.++ v3)
    v6 : v7 -> v6 : (append4._pe0 v7 v2 v3)
