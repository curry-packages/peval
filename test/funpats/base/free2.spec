module free2 ( free2.mkFree, free2.goal, free2.main ) where

import Prelude

free2.mkFree :: a -> b
free2.mkFree v1 = let v2 free in v2

free2.goal :: a -> b
free2.goal v1 = free2._pe0 v1

free2.main :: a
free2.main = (free2.goal Prelude.True)
  ?
  ((free2.goal Prelude.False) ? (free2.goal Prelude.failed))

free2._pe0 :: a -> b
free2._pe0 v1 = let v2 free in v2
