module partial ( partial.const, partial.goal, partial.main ) where

import Prelude

partial.const :: a -> b -> a
partial.const v1 v2 = v1

partial.goal :: a -> Prelude.Int
partial.goal = Prelude.apply partial.const 0

partial.main :: Prelude.Int
partial.main = Prelude.apply partial.goal Prelude.True
