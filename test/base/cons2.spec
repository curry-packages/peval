module cons2 ( cons2.m1, cons2.m2, (cons2.++), cons2.main ) where

import Prelude

cons2.m1 :: Prelude.Maybe Prelude.Int
cons2.m1 = Prelude.Just 0

cons2.m2 :: Prelude.Maybe a
cons2.m2 = Prelude.Nothing

(cons2.++) :: [a] -> [a] -> [a]
(cons2.++) v1 v2 = fcase v1 of
    [] -> v2
    v3 : v4 -> v3 : (v4 cons2.++ v2)

cons2.main :: [Prelude.Maybe Prelude.Int]
cons2.main = cons2._pe0

cons2._pe0 :: [Prelude.Maybe Prelude.Int]
cons2._pe0 = (Prelude.Just 0) : (Prelude.Nothing : [])
