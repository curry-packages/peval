module uni18 ( uni18.PEVAL, uni18.goal, uni18.main ) where

import Prelude

uni18.PEVAL :: a -> a
uni18.PEVAL v1 = v1

uni18.goal :: Prelude.Int -> Prelude.Bool
uni18.goal v1 = uni18._pe0 v1

uni18.main :: Prelude.Int
uni18.main = let v1 free in (uni18.goal v1) &> v1

uni18._pe0 :: Prelude.Int -> Prelude.Bool
uni18._pe0 v1 = fcase v1 of
    1 -> Prelude.True
