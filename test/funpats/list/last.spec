module last ( last.PEVAL, (last.++), last.last, last.main ) where

import Prelude

last.PEVAL :: a -> a
last.PEVAL v1 = v1

(last.++) :: [a] -> [a] -> [a]
(last.++) v1 v2 = fcase v1 of
    [] -> v2
    v3 : v4 -> v3 : (v4 last.++ v2)

last.last :: [a] -> a
last.last v1 = last._pe0 v1

last.main :: Prelude.Int
last.main = last.last (1 : (2 : (3 : [])))

last._pe0 :: [a] -> a
last._pe0 v1 = fcase v1 of
    v2 : v3 -> last._pe1 v3 v2

last._pe1 :: [a] -> a -> a
last._pe1 v1 v2 = fcase v1 of
    [] -> v2
    v3 : v4 -> last._pe1 v4 v3
