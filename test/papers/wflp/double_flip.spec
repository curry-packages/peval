module double_flip
  ( double_flip.TreeInt (..), double_flip.gentree, double_flip.fliptree
  , double_flip.goal, double_flip.main, double_flip.benchmark )
  where

import Prelude
import Profile

data double_flip.TreeInt
  = double_flip.Leaf Prelude.Int
  | double_flip.Tree Prelude.Int double_flip.TreeInt double_flip.TreeInt

double_flip.gentree :: Prelude.Int -> double_flip.TreeInt
double_flip.gentree v1 = case (v1 == 0) of
    Prelude.True -> double_flip.Leaf 0
    Prelude.False -> case Prelude.otherwise of
        Prelude.True -> double_flip.Tree
          v1
          (double_flip.gentree (v1 - 1))
          (double_flip.gentree (v1 - 1))
        Prelude.False -> Prelude.failed

double_flip.fliptree :: double_flip.TreeInt -> double_flip.TreeInt
double_flip.fliptree v1 = fcase v1 of
    double_flip.Leaf v2 -> double_flip.Leaf v2
    double_flip.Tree v3 v4 v5 -> double_flip.Tree
      v3
      (double_flip.fliptree v5)
      (double_flip.fliptree v4)

double_flip.goal :: double_flip.TreeInt -> double_flip.TreeInt
double_flip.goal v1 = double_flip._pe0 v1

double_flip.main :: Prelude.Bool
double_flip.main = let { v1 = double_flip.gentree 5 }
  in (double_flip.goal v1) == v1

double_flip.benchmark :: Prelude.IO ()
double_flip.benchmark = let v1 free
  in (Prelude.doSolve (v1 Prelude.=:= (double_flip.gentree 17)))
  Prelude.>>
  (Profile.profileTimeNF (double_flip.goal v1))

double_flip._pe0 :: double_flip.TreeInt -> double_flip.TreeInt
double_flip._pe0 v1 = fcase v1 of
    double_flip.Leaf v2 -> double_flip.Leaf v2
    double_flip.Tree v3 v4 v5 -> double_flip.Tree
      v3
      (double_flip._pe0 v4)
      (double_flip._pe0 v5)
