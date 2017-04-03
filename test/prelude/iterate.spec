module iterate
  ( iterate.PEVAL, iterate.head, iterate.not, iterate.iterate, iterate.main )
  where

import Prelude

iterate.PEVAL :: a -> a
iterate.PEVAL v1 = v1

iterate.head :: [a] -> a
iterate.head v1 = fcase v1 of
    v2 : v3 -> v2

iterate.not :: Prelude.Bool -> Prelude.Bool
iterate.not v1 = fcase v1 of
    Prelude.True -> Prelude.False
    Prelude.False -> Prelude.True

iterate.iterate :: (a -> a) -> a -> [a]
iterate.iterate v1 v2 = v2 : (iterate.iterate v1 (Prelude.apply v1 v2))

iterate.main :: Prelude.Bool
iterate.main = iterate._pe0

iterate._pe0 :: Prelude.Bool
iterate._pe0 = Prelude.True
