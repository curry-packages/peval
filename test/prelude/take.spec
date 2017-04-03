module take ( take.PEVAL, take.take, take.main ) where

import Prelude

take.PEVAL :: a -> a
take.PEVAL v1 = v1

take.take :: Prelude.Int -> [a] -> [a]
take.take v1 v2 = fcase v2 of
    [] -> []
    v3 : v4 -> case (v1 <= 0) of
        Prelude.True -> []
        Prelude.False -> v3 : (take.take (v1 - 1) v4)

take.main :: [Prelude.Int]
take.main = take._pe0

take._pe0 :: [Prelude.Int]
take._pe0 = 1 : []
