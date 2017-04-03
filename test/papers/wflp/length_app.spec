module length_app
  ( length_app.Nat (..), length_app.length2, length_app.append, length_app.goal
  , length_app.main, length_app.benchmark )
  where

import Prelude
import Profile

data length_app.Nat
  = length_app.Z
  | length_app.S length_app.Nat

length_app.length2 :: [a] -> length_app.Nat
length_app.length2 v1 = fcase v1 of
    [] -> length_app.Z
    v2 : v3 -> length_app.S (length_app.length2 v3)

length_app.append :: [a] -> [a] -> [a]
length_app.append v1 v2 = fcase v1 of
    [] -> v2
    v3 : v4 -> v3 : (length_app.append v4 v2)

length_app.goal :: [a] -> [a] -> length_app.Nat
length_app.goal v1 v2 = length_app._pe0 v1 v2

length_app.main :: length_app.Nat
length_app.main = length_app.goal
  (Prelude.enumFromTo 1 10)
  (Prelude.enumFromTo 11 20)

length_app.benchmark :: Prelude.IO ()
length_app.benchmark = let v1,v2 free
  in (Prelude.doSolve
  ((v1 Prelude.=:= (Prelude.enumFromTo 1 200000))
  &>
  (v2 Prelude.=:= (Prelude.enumFromTo 1 1000))))
  Prelude.>>
  (Profile.profileTimeNF (length_app.goal v1 v2))

length_app._pe0 :: [a] -> [b] -> length_app.Nat
length_app._pe0 v1 v2 = fcase v1 of
    [] -> fcase v2 of
        [] -> length_app.Z
        v3 : v4 -> length_app.S (length_app.length2 v4)
    v5 : v6 -> length_app.S (length_app._pe0 v6 v2)
