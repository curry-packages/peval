module loop ( loop.PEVAL, loop.loop, loop.main ) where

import Prelude

loop.PEVAL :: a -> a
loop.PEVAL v1 = v1

loop.loop :: a
loop.loop = loop.loop

loop.main :: a
loop.main = loop.loop
