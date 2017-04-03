module choice01 ( choice01.PEVAL, (choice01.?), choice01.main ) where

import Prelude

choice01.PEVAL :: a -> a
choice01.PEVAL v1 = v1

(choice01.?) :: a -> a -> a
(choice01.?) v1 v2 = v1 ? v2

choice01.main :: Prelude.Int
choice01.main = choice01._pe0

choice01._pe0 :: Prelude.Int
choice01._pe0 = 1 ? 2
