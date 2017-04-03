module map_var
  ( map_var.PEVAL, map_var.inc, map_var.map, map_var.goal, map_var.main )
  where

import Prelude

map_var.PEVAL :: a -> a
map_var.PEVAL v1 = v1

map_var.inc :: Prelude.Int -> Prelude.Int
map_var.inc v1 = v1 + 1

map_var.map :: (a -> b) -> [a] -> [b]
map_var.map v1 v2 = fcase v2 of
    [] -> []
    v3 : v4 -> (Prelude.apply v1 v3) : (map_var.map v1 v4)

map_var.goal :: [Prelude.Int] -> [Prelude.Int]
map_var.goal v1 = map_var._pe0 v1

map_var.main :: [Prelude.Int]
map_var.main = map_var.goal (Prelude.enumFromTo 1 10)

map_var._pe0 :: [Prelude.Int] -> [Prelude.Int]
map_var._pe0 v1 = fcase v1 of
    [] -> []
    v2 : v3 -> (v2 + 1) : (map_var._pe0 v3)
