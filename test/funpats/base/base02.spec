module base02 ( (base02.++), base02.vars, base02.main ) where

import Prelude

(base02.++) :: [a] -> [a] -> [a]
(base02.++) v1 v2 = fcase v1 of
    [] -> v2
    v3 : v4 -> v3 : (v4 base02.++ v2)

base02.vars :: [Prelude.Int] -> Prelude.Int
base02.vars v1 = base02._pe0 v1

base02.main :: Prelude.Int
base02.main = base02.vars (23 : (19 : []))

base02._pe0 :: [Prelude.Int] -> Prelude.Int
base02._pe0 v1 = fcase v1 of
    v2 : v3 -> fcase v3 of
        v4 : v5 -> fcase v5 of
            [] -> v2 + v4
