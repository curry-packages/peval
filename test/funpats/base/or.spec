module or ( or.coin, or.goal, or.main ) where

import Prelude

or.coin :: a -> Prelude.Bool
or.coin v1 = Prelude.True ? Prelude.False

or.goal :: Prelude.Bool -> Prelude.Int
or.goal v1 = or._pe0 v1

or.main :: Prelude.Int
or.main = (or.goal Prelude.True) ? (or.goal Prelude.False)

or._pe0 :: Prelude.Bool -> Prelude.Int
or._pe0 v1 = fcase v1 of
    Prelude.True -> 0
    Prelude.False -> 0
