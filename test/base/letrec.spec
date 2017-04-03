module letrec ( letrec.PEVAL, letrec.goal, letrec.main ) where

import Prelude

letrec.PEVAL :: a -> a
letrec.PEVAL v1 = v1

letrec.goal :: [Prelude.Int]
letrec.goal = letrec._pe0

letrec.main :: [Prelude.Int]
letrec.main = Prelude.take 10 letrec.goal

letrec._pe0 :: [Prelude.Int]
letrec._pe0 = 1 : (2 : letrec._pe0)
