module base01 ( (base01.++), base01.unary, base01.main ) where

import Prelude

(base01.++) :: [a] -> [a] -> [a]
(base01.++) v1 v2 = fcase v1 of
    [] -> v2
    v3 : v4 -> v3 : (v4 base01.++ v2)

base01.unary :: [a] -> Prelude.Int
base01.unary v1 = base01._pe0 v1

base01.main :: Prelude.Int
base01.main = (base01.unary []) ? (base01.unary (1 : []))

base01._pe0 :: [a] -> Prelude.Int
base01._pe0 v1 = fcase v1 of
    [] -> 42
