module head ( head.PEVAL, head.head, head.main ) where

import Prelude

head.PEVAL :: a -> a
head.PEVAL v1 = v1

head.head :: [a] -> a
head.head v1 = fcase v1 of
    v2 : v3 -> v2

head.main :: Prelude.Int
head.main = head._pe0

head._pe0 :: Prelude.Int
head._pe0 = 1
