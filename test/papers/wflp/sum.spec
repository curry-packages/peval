module sum ( sum.foldr, sum.goal, sum.main, sum.benchmark ) where

import Prelude
import Profile

sum.foldr :: (a -> b -> b) -> b -> [a] -> b
sum.foldr v1 v2 v3 = fcase v3 of
    [] -> v2
    v4 : v5 -> Prelude.apply (Prelude.apply v1 v4) (sum.foldr v1 v2 v5)

sum.goal :: [Prelude.Int] -> Prelude.Int
sum.goal v1 = sum._pe0 v1

sum.main :: Prelude.Int
sum.main = sum.goal (Prelude.enumFromTo 1 10)

sum.benchmark :: Prelude.IO ()
sum.benchmark = let v1 free
  in (Prelude.doSolve (v1 Prelude.=:= (Prelude.enumFromTo 1 200000)))
  Prelude.>>
  (Profile.profileTimeNF (sum.goal v1))

sum._pe0 :: [Prelude.Int] -> Prelude.Int
sum._pe0 v1 = fcase v1 of
    [] -> 0
    v2 : v3 -> v2 + (sum._pe0 v3)
