module filtermap
  ( filtermap.flip, filtermap.map, filtermap.filter, filtermap.goal
  , filtermap.main, filtermap.benchmark )
  where

import Prelude

filtermap.flip :: (a -> b -> c) -> b -> a -> c
filtermap.flip v1 v2 v3 = Prelude.apply (Prelude.apply v1 v3) v2

filtermap.map :: (a -> b) -> [a] -> [b]
filtermap.map v1 v2 = fcase v2 of
    [] -> []
    v3 : v4 -> (Prelude.apply v1 v3) : (filtermap.map v1 v4)

filtermap.filter :: (a -> Prelude.Bool) -> [a] -> [a]
filtermap.filter v1 v2 = fcase v2 of
    [] -> []
    v3 : v4 -> case (Prelude.apply v1 v3) of
        Prelude.True -> v3 : (filtermap.filter v1 v4)
        Prelude.False -> filtermap.filter v1 v4

filtermap.goal :: [Prelude.Int] -> [Prelude.Int]
filtermap.goal v1 = filtermap._pe0 v1

filtermap.main :: [Prelude.Int]
filtermap.main = filtermap.goal (Prelude.enumFromThenTo 10 20 200)

filtermap.benchmark :: Prelude.Bool
filtermap.benchmark = let v1,v2 free
  in (v1 Prelude.=:= (Prelude.enumFromTo 1 20000))
  &>
  ((filtermap.goal v1) Prelude.=:= v2)

filtermap._pe0 :: [Prelude.Int] -> [Prelude.Int]
filtermap._pe0 v1 = fcase v1 of
    [] -> []
    v2 : v3 -> let { v4 = v2 * 3 }
      in case (v4 > 100) of
        Prelude.True -> v4 : (filtermap._pe0 v3)
        Prelude.False -> filtermap._pe0 v3
