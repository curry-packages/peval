module digit ( digit.digit, digit.goal, digit.goal1, digit.main ) where

import Prelude

digit.digit :: Prelude.Int -> Prelude.Bool
digit.digit v1 = fcase v1 of
    0 -> Prelude.success
    1 -> Prelude.success
    2 -> Prelude.success
    3 -> Prelude.success
    4 -> Prelude.success
    5 -> Prelude.success
    6 -> Prelude.success
    7 -> Prelude.success
    8 -> Prelude.success
    9 -> Prelude.success

digit.goal :: Prelude.Int -> Prelude.Int -> Prelude.Bool
digit.goal v1 v2 = ((v1 + v1) Prelude.=:= v2)
  &
  (((v1 * v1) Prelude.=:= v2) & (digit.digit v1))

digit.goal1 :: Prelude.Int -> Prelude.Int -> Prelude.Bool
digit.goal1 v1 v2 = digit._pe0 v1 v2

digit.main :: (Prelude.Int,Prelude.Int)
digit.main = let v1,v2 free in (digit.goal1 v1 v2) &> (v1,v2)

digit._pe0 :: Prelude.Int -> Prelude.Int -> Prelude.Bool
digit._pe0 v1 v2 = fcase v1 of
    0 -> fcase v2 of
        0 -> Prelude.True
    2 -> fcase v2 of
        4 -> Prelude.True
