module twice_square
  ( twice_square.map, twice_square.twice, twice_square.square, twice_square.goal
  , twice_square.main, twice_square.benchmark )
  where

import Prelude
import Profile

twice_square.map :: (a -> b) -> [a] -> [b]
twice_square.map v1 v2 = fcase v2 of
    [] -> []
    v3 : v4 -> (Prelude.apply v1 v3) : (twice_square.map v1 v4)

twice_square.twice :: (a -> a) -> a -> a
twice_square.twice v1 v2 = Prelude.apply v1 (Prelude.apply v1 v2)

twice_square.square :: Prelude.Int -> Prelude.Int
twice_square.square v1 = v1 * v1

twice_square.goal :: [Prelude.Int] -> [Prelude.Int]
twice_square.goal v1 = twice_square._pe0 v1

twice_square.main :: [Prelude.Int]
twice_square.main = twice_square.goal (Prelude.enumFromTo 1 10)

twice_square.benchmark :: Prelude.IO ()
twice_square.benchmark = let v1 free
  in (Prelude.doSolve (v1 Prelude.=:= (Prelude.enumFromTo 1 200000)))
  Prelude.>>
  (Profile.profileTimeNF (twice_square.goal v1))

twice_square._pe0 :: [Prelude.Int] -> [Prelude.Int]
twice_square._pe0 v1 = fcase v1 of
    [] -> []
    v2 : v3 -> (let { v4 = v2 * v2 } in v4 * v4) : (twice_square._pe0 v3)
