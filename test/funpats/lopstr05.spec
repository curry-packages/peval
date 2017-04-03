module lopstr05 ( lopstr05.k0, lopstr05.pair, lopstr05.f, lopstr05.main ) where

import Prelude

lopstr05.k0 :: a -> Prelude.Int
lopstr05.k0 v1 = 0

lopstr05.pair :: a -> b -> (a,b)
lopstr05.pair v1 v2 = (v1,v2)

lopstr05.f :: (Prelude.Int,a) -> Prelude.Int
lopstr05.f v1 = lopstr05._pe0 v1

lopstr05.main :: Prelude.Int
lopstr05.main = lopstr05.f (0,Prelude.failed)

lopstr05._pe0 :: (Prelude.Int,a) -> Prelude.Int
lopstr05._pe0 v1 = fcase v1 of
    (v2,v3) -> fcase v2 of
        0 -> 0
