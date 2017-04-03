module sum_square
  ( sum_square.map, sum_square.foldr, sum_square.square, sum_square.goal
  , sum_square.main, sum_square.benchmark )
  where

import Prelude
import Profile

sum_square.map :: (a -> b) -> [a] -> [b]
sum_square.map v1 v2 = fcase v2 of
    [] -> []
    v3 : v4 -> (Prelude.apply v1 v3) : (sum_square.map v1 v4)

sum_square.foldr :: (a -> b -> b) -> b -> [a] -> b
sum_square.foldr v1 v2 v3 = fcase v3 of
    [] -> v2
    v4 : v5 -> Prelude.apply (Prelude.apply v1 v4) (sum_square.foldr v1 v2 v5)

sum_square.square :: Prelude.Int -> Prelude.Int
sum_square.square v1 = v1 * v1

sum_square.goal :: [Prelude.Int] -> Prelude.Int
sum_square.goal v1 = sum_square._pe0 v1

sum_square.main :: Prelude.Int
sum_square.main = sum_square.goal (Prelude.enumFromTo 1 10)

sum_square.benchmark :: Prelude.IO ()
sum_square.benchmark = let v1 free
  in (Prelude.doSolve (v1 Prelude.=:= (Prelude.enumFromTo 1 200000)))
  Prelude.>>
  (Profile.profileTimeNF (sum_square.goal v1))

sum_square._pe0 :: [Prelude.Int] -> Prelude.Int
sum_square._pe0 v1 = fcase v1 of
    [] -> 0
    v2 : v3 -> (v2 * v2) + (sum_square._pe0 v3)
