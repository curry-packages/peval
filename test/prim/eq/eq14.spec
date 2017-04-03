module eq14 ( eq14.PEVAL, eq14.main ) where

import Prelude

eq14.PEVAL :: a -> a
eq14.PEVAL v1 = v1

eq14.main :: Prelude.Bool
eq14.main = eq14._pe0

eq14._pe0 :: Prelude.Bool
eq14._pe0 = (let v1 free in v1) == 1
