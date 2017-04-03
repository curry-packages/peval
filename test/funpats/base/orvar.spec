module orvar ( orvar.coin, orvar.goal, orvar.main ) where

import Prelude

orvar.coin :: Prelude.Bool -> Prelude.Bool
orvar.coin v1 = Prelude.True ? Prelude.False

orvar.goal :: Prelude.Bool -> Prelude.Bool
orvar.goal v1 = orvar._pe0 v1

orvar.main :: Prelude.Bool
orvar.main = (orvar.goal Prelude.True) ? (orvar.goal Prelude.False)

orvar._pe0 :: Prelude.Bool -> a
orvar._pe0 v1 = fcase v1 of
    Prelude.True -> let v2 free in v2
    Prelude.False -> let v3 free in v3
