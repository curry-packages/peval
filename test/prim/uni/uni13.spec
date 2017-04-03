module uni13 ( uni13.PEVAL, uni13.goal, uni13.main ) where

import Prelude

uni13.PEVAL :: a -> a
uni13.PEVAL v1 = v1

uni13.goal :: Prelude.Bool -> Prelude.Bool
uni13.goal v1 = uni13._pe0 v1

uni13.main :: Prelude.Bool
uni13.main = (uni13.goal Prelude.True) ? (uni13.goal Prelude.False)

uni13._pe0 :: Prelude.Bool -> Prelude.Bool
uni13._pe0 v1 = case v1 of
    Prelude.True -> Prelude.True
