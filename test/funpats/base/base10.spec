module base10 ( base10.dpair, base10.goal, base10.main ) where

import Prelude

base10.dpair :: a -> (a,a)
base10.dpair v1 = (v1,v1)

base10.goal :: (a,a) -> a
base10.goal v1 = base10._pe0 v1

base10.main :: Prelude.Int
base10.main = (base10.goal (1,1)) ? (base10.goal (1,2))

base10._pe0 :: (a,a) -> a
base10._pe0 v1 = fcase v1 of
    (v2,v3) -> (v2 Prelude.=:= v3) &> v2
