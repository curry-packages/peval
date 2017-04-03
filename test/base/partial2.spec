module partial2 ( partial2.const, partial2.goal, partial2.main ) where

import Prelude

partial2.const :: a -> b -> a
partial2.const v1 v2 = v1

partial2.goal :: a -> Prelude.Int
partial2.goal = partial2._pe0

partial2.main :: Prelude.Int
partial2.main = Prelude.apply partial2.goal Prelude.True

partial2._pe0 :: a -> Prelude.Int
partial2._pe0 v1 = 0
