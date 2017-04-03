module loop2 ( loop2.PEVAL, loop2.loop, (loop2.&&), loop2.main ) where

import Prelude

loop2.PEVAL :: a -> a
loop2.PEVAL v1 = v1

loop2.loop :: a
loop2.loop = loop2.loop

(loop2.&&) :: Prelude.Bool -> Prelude.Bool -> Prelude.Bool
(loop2.&&) v1 v2 = fcase v1 of
    Prelude.False -> Prelude.False
    Prelude.True -> v2

loop2.main :: Prelude.Bool
loop2.main = loop2._pe0

loop2._pe0 :: Prelude.Bool
loop2._pe0 = fcase loop2.loop of
    Prelude.False -> Prelude.False
    Prelude.True -> Prelude.False
