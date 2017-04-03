module plus ( plus.f, plus.main ) where

import Prelude

plus.f :: Prelude.Int -> Prelude.Int
plus.f v1 = plus._pe0 v1

plus.main :: (Prelude.Int,Prelude.Int)
plus.main = (plus.f 0,plus.f 1)

plus._pe0 :: Prelude.Int -> Prelude.Int
plus._pe0 v1 = fcase v1 of
    0 -> 1
    1 -> 3
