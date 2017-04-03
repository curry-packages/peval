module allones
  ( allones.Nat (..), allones.allones, allones.length, allones.goal
  , allones.main, allones.benchmark )
  where

import Prelude
import Profile

data allones.Nat
  = allones.Z
  | allones.S allones.Nat

allones.allones :: allones.Nat -> [Prelude.Int]
allones.allones v1 = fcase v1 of
    allones.Z -> []
    allones.S v2 -> 1 : (allones.allones v2)

allones.length :: [a] -> allones.Nat
allones.length v1 = fcase v1 of
    [] -> allones.Z
    v2 : v3 -> allones.S (allones.length v3)

allones.goal :: [a] -> [Prelude.Int]
allones.goal v1 = allones._pe0 v1

allones.main :: [Prelude.Int]
allones.main = allones.goal (Prelude.enumFromTo 1 10)

allones.benchmark :: Prelude.IO ()
allones.benchmark = let v1 free
  in (Prelude.doSolve (v1 Prelude.=:= (Prelude.enumFromTo 1 200000)))
  Prelude.>>
  (Profile.profileTimeNF (allones.goal v1))

allones._pe0 :: [a] -> [Prelude.Int]
allones._pe0 v1 = fcase v1 of
    [] -> []
    v2 : v3 -> 1 : (allones._pe0 v3)
