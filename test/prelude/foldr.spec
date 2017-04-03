module foldr ( foldr.PEVAL, foldr.foldr, foldr.main ) where

import Prelude

foldr.PEVAL :: a -> a
foldr.PEVAL v1 = v1

foldr.foldr :: (a -> b -> b) -> b -> [a] -> b
foldr.foldr v1 v2 v3 = fcase v3 of
    [] -> v2
    v4 : v5 -> Prelude.apply (Prelude.apply v1 v4) (foldr.foldr v1 v2 v5)

foldr.main :: Prelude.Int
foldr.main = foldr._pe0

foldr._pe0 :: Prelude.Int
foldr._pe0 = 6
