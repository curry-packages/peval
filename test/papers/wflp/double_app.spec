module double_app
  ( (double_app.++), double_app.goal, double_app.main, double_app.benchmark )
  where

import Prelude
import Profile

(double_app.++) :: [a] -> [a] -> [a]
(double_app.++) v1 v2 = fcase v1 of
    [] -> v2
    v3 : v4 -> v3 : (v4 double_app.++ v2)

double_app.goal :: [a] -> [a] -> [a] -> [a]
double_app.goal v1 v2 v3 = double_app._pe0 v1 v2 v3

double_app.main :: [Prelude.Int]
double_app.main = double_app.goal
  (Prelude.enumFromTo 1 3)
  (Prelude.enumFromTo 4 6)
  (Prelude.enumFromTo 7 10)

double_app.benchmark :: Prelude.IO ()
double_app.benchmark = let v1,v2 free
  in (Prelude.doSolve (v1 Prelude.=:= (Prelude.enumFromTo 1 200000)))
  Prelude.>>
  ((Prelude.doSolve (v2 Prelude.=:= (Prelude.enumFromTo 1 100)))
  Prelude.>>
  (Profile.profileTimeNF (double_app.goal v1 v2 v2)))

double_app._pe0 :: [a] -> [a] -> [a] -> [a]
double_app._pe0 v1 v2 v3 = fcase v1 of
    [] -> fcase v2 of
        [] -> v3
        v4 : v5 -> v4 : (v5 double_app.++ v3)
    v6 : v7 -> v6 : (double_app._pe0 v7 v2 v3)
