module cond16 ( cond16.PEVAL, cond16.goal, cond16.main ) where

import Prelude

cond16.PEVAL :: a -> a
cond16.PEVAL v1 = v1

cond16.goal :: Prelude.Bool -> Prelude.Bool
cond16.goal v1 = cond16._pe0 v1

cond16.main :: Prelude.Bool
cond16.main = (cond16.goal Prelude.True) ? (cond16.goal Prelude.False)

cond16._pe0 :: Prelude.Bool -> Prelude.Bool
cond16._pe0 v1 = case v1 of
    Prelude.True -> Prelude.True
