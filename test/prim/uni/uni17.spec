module uni17 ( uni17.PEVAL, uni17.goal, uni17.main ) where

import Prelude

uni17.PEVAL :: a -> a
uni17.PEVAL v1 = v1

uni17.goal :: Prelude.Int -> Prelude.Bool
uni17.goal v1 = uni17._pe0 v1

uni17.main :: Prelude.Int
uni17.main = let v1 free in (uni17.goal v1) &> v1

uni17._pe0 :: Prelude.Int -> Prelude.Bool
uni17._pe0 v1 = fcase v1 of
    1 -> Prelude.True
