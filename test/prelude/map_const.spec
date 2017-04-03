module map_const
  ( map_const.map, map_const.const, map_const.goal, map_const.main )
  where

import Prelude

map_const.map :: (a -> b) -> [a] -> [b]
map_const.map v1 v2 = fcase v2 of
    [] -> []
    v3 : v4 -> (Prelude.apply v1 v3) : (map_const.map v1 v4)

map_const.const :: a -> b -> a
map_const.const v1 v2 = v1

map_const.goal :: [a] -> [Prelude.Int]
map_const.goal v1 = map_const._pe0 v1

map_const.main :: [Prelude.Int]
map_const.main = map_const.goal (Prelude.enumFromTo 1 10)

map_const._pe0 :: [a] -> [Prelude.Int]
map_const._pe0 v1 = fcase v1 of
    [] -> []
    v2 : v3 -> 1 : (map_const._pe0 v3)
