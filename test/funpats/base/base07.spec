module base07 ( base07.id, base07.mkPair, base07.nonLinear, base07.main ) where

import Prelude

base07.id :: a -> a
base07.id v1 = v1

base07.mkPair :: a -> b -> (a,b)
base07.mkPair v1 v2 = (base07.id v1,base07.id v2)

base07.nonLinear :: (Prelude.Int,Prelude.Int) -> Prelude.Int
base07.nonLinear v1 = base07._pe0 v1

base07.main :: Prelude.Int
base07.main = (base07.nonLinear (1,1)) ? (base07.nonLinear (1,2))

base07._pe0 :: (Prelude.Int,Prelude.Int) -> Prelude.Int
base07._pe0 v1 = fcase v1 of
    (v2,v3) -> (v2 Prelude.=:= v3) &> (v2 + v2)
