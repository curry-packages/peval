module append2 ( append2.PEVAL, (append2.++), append2.goal, append2.main ) where

import Prelude

append2.PEVAL :: a -> a
append2.PEVAL v1 = v1

(append2.++) :: [a] -> [a] -> [a]
(append2.++) v1 v2 = fcase v1 of
    [] -> v2
    v3 : v4 -> v3 : (v4 append2.++ v2)

append2.goal :: [a]
append2.goal = append2._pe0

append2.main :: Prelude.Bool
append2.main = Prelude.null append2.goal

append2._pe0 :: [a]
append2._pe0 = [] ? ((let v1 free in v1) : append2._pe0)
