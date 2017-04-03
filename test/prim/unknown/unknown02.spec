module unknown02 ( unknown02.PEVAL, (unknown02.&&), unknown02.main ) where

import Prelude

unknown02.PEVAL :: a -> a
unknown02.PEVAL v1 = v1

(unknown02.&&) :: Prelude.Bool -> Prelude.Bool -> Prelude.Bool
(unknown02.&&) v1 v2 = fcase v1 of
    Prelude.False -> Prelude.False
    Prelude.True -> v2

unknown02.main :: Prelude.Bool
unknown02.main = unknown02._pe0

unknown02._pe0 :: Prelude.Bool
unknown02._pe0 = Prelude.False ? (let v1 free in v1)
