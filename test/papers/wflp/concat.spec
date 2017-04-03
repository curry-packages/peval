module concat
  ( (concat.++), concat.foldr, concat.goal, concat.main, concat.benchmark )
  where

import Prelude
import Profile

(concat.++) :: [a] -> [a] -> [a]
(concat.++) v1 v2 = fcase v1 of
    [] -> v2
    v3 : v4 -> v3 : (v4 concat.++ v2)

concat.foldr :: (a -> b -> b) -> b -> [a] -> b
concat.foldr v1 v2 v3 = fcase v3 of
    [] -> v2
    v4 : v5 -> Prelude.apply (Prelude.apply v1 v4) (concat.foldr v1 v2 v5)

concat.goal :: [[a]] -> [a]
concat.goal v1 = concat._pe0 v1

concat.main :: [Prelude.Int]
concat.main = concat.goal
  ((Prelude.enumFromTo 1 3)
  :
  ((Prelude.enumFromTo 4 6) : ((Prelude.enumFromTo 7 10) : [])))

concat.benchmark :: Prelude.IO ()
concat.benchmark = let v1 free
  in (Prelude.doSolve
  (v1
  Prelude.=:=
  (Prelude.map concat.benchmark._#lambda1 (Prelude.enumFromTo 1 200000))))
  Prelude.>>
  (Profile.profileTimeNF (concat.goal v1))

concat.benchmark._#lambda1 :: Prelude.Int -> [Prelude.Int]
concat.benchmark._#lambda1 v1 = v1 : []

concat._pe0 :: [[a]] -> [a]
concat._pe0 v1 = fcase v1 of
    [] -> []
    v2 : v3 -> concat._pe1 v2 v3

concat._pe1 :: [a] -> [[a]] -> [a]
concat._pe1 v1 v2 = fcase v1 of
    [] -> fcase v2 of
        [] -> []
        v3 : v4 -> concat._pe1 v3 v4
    v5 : v6 -> v5 : (concat._pe1 v6 v2)
