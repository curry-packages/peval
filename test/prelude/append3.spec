module append3 ( append3.PEVAL, (append3.++), append3.goal, append3.main ) where

import Prelude

append3.PEVAL :: a -> a
append3.PEVAL v1 = v1

(append3.++) :: [a] -> [a] -> [a]
(append3.++) v1 v2 = fcase v1 of
    [] -> v2
    v3 : v4 -> v3 : (v4 append3.++ v2)

append3.goal :: [a] -> [a] -> [a] -> [a]
append3.goal v1 v2 v3 = append3._pe0 v1 v2 v3

append3.main :: [Prelude.Int]
append3.main = append3.goal
  (Prelude.enumFromTo 1 3)
  (Prelude.enumFromTo 4 6)
  (Prelude.enumFromTo 7 10)

append3._pe0 :: [a] -> [a] -> [a] -> [a]
append3._pe0 v1 v2 v3 = fcase v1 of
    [] -> v2 append3.++ v3
    v4 : v5 -> v4 : (append3._pe0 v5 v2 v3)
