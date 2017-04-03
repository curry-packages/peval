module ones ( ones.PEVAL, ones.goal, ones.main ) where

import Prelude

ones.PEVAL :: a -> a
ones.PEVAL v1 = v1

ones.goal :: [Prelude.Int]
ones.goal = ones._pe0

ones.main :: [Prelude.Int]
ones.main = Prelude.take 10 ones.goal

ones._pe0 :: [Prelude.Int]
ones._pe0 = 1 : ones._pe0
