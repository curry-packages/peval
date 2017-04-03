module base06 ( (base06.++), base06.fstFP, base06.main ) where

import Prelude

(base06.++) :: [a] -> [a] -> [a]
(base06.++) v1 v2 = fcase v1 of
    [] -> v2
    v3 : v4 -> v3 : (v4 base06.++ v2)

base06.fstFP :: [a] -> [b] -> Prelude.Int
base06.fstFP v1 v2 = fcase v2 of
    [] -> base06._pe0 v1

base06.main :: Prelude.Int
base06.main = (base06.fstFP [] []) ? (base06.fstFP (1 : []) [])

base06._pe0 :: [a] -> Prelude.Int
base06._pe0 v1 = fcase v1 of
    [] -> 42
