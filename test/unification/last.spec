module last ( (last.++), last.last, last.goal, last.main ) where

import Prelude

(last.++) :: [a] -> [a] -> [a]
(last.++) v1 v2 = fcase v1 of
    [] -> v2
    v3 : v4 -> v3 : (v4 last.++ v2)

last.last :: [a] -> a
last.last v1 = let v2,v3 free
  in Prelude.cond ((v2 last.++ (v3 : [])) Prelude.=:= v1) v3

last.goal :: [a] -> a
last.goal v1 = last._pe0 v1

last.main :: Prelude.Int
last.main = (last.goal (Prelude.enumFromTo 1 10))
  ?
  (last.goal (Prelude.failed : (1 : [])))

last._pe0 :: [a] -> a
last._pe0 v1 = fcase v1 of
    v2 : v3 -> (v2 Prelude.=:= v2) &> ((fcase v3 of [] -> v2) ? (last._pe0 v3))
