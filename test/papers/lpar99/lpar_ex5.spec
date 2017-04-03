module lpar_ex5
  ( lpar_ex5.ABC (..), lpar_ex5.f, lpar_ex5.g, lpar_ex5.h, lpar_ex5.goal
  , lpar_ex5.main )
  where

import Prelude

data lpar_ex5.ABC
  = lpar_ex5.A
  | lpar_ex5.B
  | lpar_ex5.C

lpar_ex5.f :: lpar_ex5.ABC -> lpar_ex5.ABC -> lpar_ex5.ABC
lpar_ex5.f v1 v2 = fcase v1 of
    lpar_ex5.A -> fcase v2 of
        lpar_ex5.B -> lpar_ex5.C

lpar_ex5.g :: lpar_ex5.ABC -> lpar_ex5.ABC -> lpar_ex5.ABC
lpar_ex5.g v1 v2 = case v1 of
    lpar_ex5.B -> case v2 of
        lpar_ex5.C -> lpar_ex5.B

lpar_ex5.h :: lpar_ex5.ABC -> lpar_ex5.ABC
lpar_ex5.h v1 = fcase v1 of
    lpar_ex5.C -> lpar_ex5.C

lpar_ex5.goal :: lpar_ex5.ABC -> lpar_ex5.ABC -> lpar_ex5.ABC -> lpar_ex5.ABC
lpar_ex5.goal v1 v2 v3 = lpar_ex5._pe0 v1 v2 v3

lpar_ex5.main :: (lpar_ex5.ABC,lpar_ex5.ABC,lpar_ex5.ABC,lpar_ex5.ABC)
lpar_ex5.main = let v1,v2,v3 free in (lpar_ex5.goal v1 v2 v3,v1,v2,v3)

lpar_ex5._pe0 :: lpar_ex5.ABC -> lpar_ex5.ABC -> lpar_ex5.ABC -> lpar_ex5.ABC
lpar_ex5._pe0 v1 v2 v3 = fcase v1 of
    lpar_ex5.A -> case v2 of
        lpar_ex5.B -> fcase v3 of
            lpar_ex5.C -> lpar_ex5.C
