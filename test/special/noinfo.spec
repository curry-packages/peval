module noinfo ( noinfo.map, noinfo.goal, noinfo.main ) where

import Prelude

noinfo.map :: (a -> Prelude.Int) -> [a] -> [Prelude.Int]
noinfo.map v1 v2 = fcase v2 of
    [] -> []
    v3 : v4 -> ((Prelude.apply v1 v3) + (1 + 1)) : (noinfo.map v1 v4)

noinfo.goal :: (a -> Prelude.Int) -> [a] -> [Prelude.Int]
noinfo.goal v1 v2 = noinfo._pe0 v1 v2

noinfo.main :: [Prelude.Int]
noinfo.main = noinfo.goal (Prelude.flip (+) 1) (Prelude.enumFromTo 1 10)

noinfo._pe0 :: (a -> Prelude.Int) -> [a] -> [Prelude.Int]
noinfo._pe0 v1 v2 = fcase v2 of
    [] -> []
    v3 : v4 -> ((Prelude.apply v1 v3) + 2) : (noinfo._pe0 v1 v4)
