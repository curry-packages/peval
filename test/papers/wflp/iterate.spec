module iterate
  ( iterate.map, iterate.comp, iterate.inc, iterate.iter, iterate.goal
  , iterate.main, iterate.benchmark )
  where

import Prelude
import Profile

iterate.map :: (a -> b) -> [a] -> [b]
iterate.map v1 v2 = fcase v2 of
    [] -> []
    v3 : v4 -> (Prelude.apply v1 v3) : (iterate.map v1 v4)

iterate.comp :: (a -> b) -> (c -> a) -> c -> b
iterate.comp v1 v2 v3 = Prelude.apply v1 (Prelude.apply v2 v3)

iterate.inc :: Prelude.Int -> Prelude.Int
iterate.inc v1 = v1 + 1

iterate.iter :: (a -> a) -> Prelude.Int -> a -> a
iterate.iter v1 v2 = case (v2 == 0) of
    Prelude.True -> v1
    Prelude.False -> iterate.iter (iterate.comp v1 v1) (v2 - 1)

iterate.goal :: [Prelude.Int] -> [Prelude.Int]
iterate.goal v1 = iterate._pe0 v1

iterate.main :: [Prelude.Int]
iterate.main = iterate.goal (Prelude.enumFromTo 1 10)

iterate.benchmark :: Prelude.IO ()
iterate.benchmark = let v1 free
  in (Prelude.doSolve (v1 Prelude.=:= (Prelude.enumFromTo 1 200000)))
  Prelude.>>
  (Profile.profileTimeNF (iterate.goal v1))

iterate._pe0 :: [Prelude.Int] -> [Prelude.Int]
iterate._pe0 v1 = fcase v1 of
    [] -> []
    v2 : v3 -> (iterate._pe1 v2) : (iterate.map iterate._pe1 v3)

iterate._pe1 :: Prelude.Int -> Prelude.Int
iterate._pe1 v1 = (((v1 + 1) + 1) + 1) + 1
