module cond11 ( cond11.PEVAL, cond11.goal, cond11.main ) where

import Prelude

cond11.PEVAL :: a -> a
cond11.PEVAL v1 = v1

cond11.goal :: a -> a
cond11.goal v1 = cond11._pe0 v1

cond11.main :: Prelude.Int
cond11.main = cond11.goal 1

cond11._pe0 :: a -> a
cond11._pe0 v1 = v1
