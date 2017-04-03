module cond09 ( cond09.PEVAL, cond09.main ) where

import Prelude

cond09.PEVAL :: a -> a
cond09.PEVAL v1 = v1

cond09.main :: Prelude.Bool
cond09.main = cond09._pe0

cond09._pe0 :: Prelude.Bool
cond09._pe0 = (let v1 free in v1) &> Prelude.success
