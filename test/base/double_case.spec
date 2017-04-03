module double_case
  ( double_case.double, double_case.goal, double_case.main )
  where

import Prelude

double_case.double :: a -> (a,a)
double_case.double v1 = (v1,v1)

double_case.goal :: () -> (Prelude.Int,Prelude.Int)
double_case.goal v1 = double_case._pe0 v1

double_case.main :: (Prelude.Int,Prelude.Int)
double_case.main = double_case.goal ()

double_case._pe0 :: () -> (Prelude.Int,Prelude.Int)
double_case._pe0 v1 = let { v2 = case v1 of () -> 0 ? 1 } in (v2,v2)
