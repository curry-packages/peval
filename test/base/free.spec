module free ( free.PEVAL, free.id, free.main ) where

import Prelude

free.PEVAL :: a -> a
free.PEVAL v1 = v1

free.id :: a -> a
free.id v1 = v1

free.main :: a
free.main = free._pe0

free._pe0 :: a
free._pe0 = let v1 free in v1
