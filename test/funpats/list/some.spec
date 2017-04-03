module some ( some.PEVAL, (some.++), some.some, some.main ) where

import Prelude

some.PEVAL :: a -> a
some.PEVAL v1 = v1

(some.++) :: [a] -> [a] -> [a]
(some.++) v1 v2 = fcase v1 of
    [] -> v2
    v3 : v4 -> v3 : (v4 some.++ v2)

some.some :: [a] -> a
some.some v1 = some._pe0 v1

some.main :: Prelude.Int
some.main = some.some (1 : (2 : (3 : [])))

some._pe0 :: [a] -> a
some._pe0 v1 = fcase v1 of
    v2 : v3 -> some._pe1 v2 v3

some._pe1 :: a -> [a] -> a
some._pe1 v1 v2 = v1 ? (fcase v2 of
    v3 : v4 -> some._pe1 v3 v4)
