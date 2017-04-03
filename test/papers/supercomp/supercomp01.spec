module supercomp01 ( supercomp01.ones, supercomp01.map, supercomp01.main ) where

import Prelude

supercomp01.ones :: [Prelude.Int]
supercomp01.ones = 1 : supercomp01.ones

supercomp01.map :: (a -> b) -> [a] -> [b]
supercomp01.map v1 v2 = fcase v2 of
    [] -> []
    v3 : v4 -> (Prelude.apply v1 v3) : (supercomp01.map v1 v4)

supercomp01.main :: [Prelude.Int]
supercomp01.main = supercomp01._pe0

supercomp01.main._#lambda1 :: Prelude.Int -> Prelude.Int
supercomp01.main._#lambda1 v1 = v1 + 1

supercomp01._pe0 :: [Prelude.Int]
supercomp01._pe0 = 2 : supercomp01._pe0
