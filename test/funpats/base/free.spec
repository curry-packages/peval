module free ( free.mkFree, free.goal, free.main ) where

import Prelude

free.mkFree :: a -> b
free.mkFree v1 = let v2 free in v2

free.goal :: a -> Prelude.Int
free.goal v1 = free._pe0 v1

free.main :: (Prelude.Int,Prelude.Int)
free.main = (free.goal Prelude.True,free.goal Prelude.failed)

free._pe0 :: a -> Prelude.Int
free._pe0 v1 = 42
