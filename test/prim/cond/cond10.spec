module cond10 ( cond10.PEVAL, cond10.main ) where

import Prelude

cond10.PEVAL :: a -> a
cond10.PEVAL v1 = v1

cond10.main :: a
cond10.main = cond10._pe0

cond10._pe0 :: a
cond10._pe0 = (let v1 free in v1) &> Prelude.failed
