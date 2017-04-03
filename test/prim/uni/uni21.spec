module uni21 ( uni21.PEVAL, uni21.goal, uni21.main ) where

import Prelude

uni21.PEVAL :: a -> a
uni21.PEVAL v1 = v1

uni21.goal :: a -> a
uni21.goal v1 = uni21._pe0 v1

uni21.main :: Prelude.Bool
uni21.main = (uni21.goal Prelude.True) ? (uni21.goal Prelude.failed)

uni21._pe0 :: a -> a
uni21._pe0 v1 = (v1 Prelude.=:= v1) &> v1
