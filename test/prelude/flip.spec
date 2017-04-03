module flip ( flip.PEVAL, flip.flip, flip.main ) where

import Prelude

flip.PEVAL :: a -> a
flip.PEVAL v1 = v1

flip.flip :: (a -> b -> c) -> b -> a -> c
flip.flip v1 v2 v3 = Prelude.apply (Prelude.apply v1 v3) v2

flip.main :: Prelude.Int
flip.main = flip._pe0

flip._pe0 :: Prelude.Int
flip._pe0 = 42
