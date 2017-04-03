module map ( map.PEVAL, map.inc, map.map, map.main ) where

import Prelude

map.PEVAL :: a -> a
map.PEVAL v1 = v1

map.inc :: Prelude.Int -> Prelude.Int
map.inc v1 = v1 + 1

map.map :: (a -> b) -> [a] -> [b]
map.map v1 v2 = fcase v2 of
    [] -> []
    v3 : v4 -> (Prelude.apply v1 v3) : (map.map v1 v4)

map.main :: [Prelude.Int]
map.main = map._pe0

map._pe0 :: [Prelude.Int]
map._pe0 = 2 : (3 : (4 : []))
