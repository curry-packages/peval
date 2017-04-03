module foldl ( foldl.PEVAL, foldl.foldl, foldl.main ) where

import Prelude

foldl.PEVAL :: a -> a
foldl.PEVAL v1 = v1

foldl.foldl :: (a -> b -> a) -> a -> [b] -> a
foldl.foldl v1 v2 v3 = fcase v3 of
    [] -> v2
    v4 : v5 -> foldl.foldl v1 (Prelude.apply (Prelude.apply v1 v2) v4) v5

foldl.main :: Prelude.Int
foldl.main = foldl._pe0

foldl._pe0 :: Prelude.Int
foldl._pe0 = 6
