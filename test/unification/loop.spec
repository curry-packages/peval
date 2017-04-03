module loop ( loop.PEVAL, loop.goal, loop.main ) where

import Prelude

loop.PEVAL :: a -> a
loop.PEVAL v1 = v1

loop.goal :: Prelude.Bool
loop.goal = loop._pe0

loop.main :: Prelude.Bool
loop.main = loop.goal

loop._pe0 :: Prelude.Bool
loop._pe0 = (1 Prelude.=:= 1) & loop._pe0
