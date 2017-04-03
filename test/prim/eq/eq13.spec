module eq13 ( eq13.PEVAL, eq13.goal, eq13.main ) where

import Prelude

eq13.PEVAL :: a -> a
eq13.PEVAL v1 = v1

eq13.goal :: Prelude.Bool -> Prelude.Bool
eq13.goal v1 = eq13._pe0 v1

eq13.main :: (Prelude.Bool,Prelude.Bool)
eq13.main = (eq13.goal Prelude.True,eq13.goal Prelude.False)

eq13._pe0 :: Prelude.Bool -> Prelude.Bool
eq13._pe0 v1 = case v1 of
    Prelude.True -> Prelude.True
    Prelude.False -> Prelude.False
