module io ( io.PEVAL, io.main ) where

import Prelude

io.PEVAL :: a -> a
io.PEVAL v1 = v1

io.main :: Prelude.IO ()
io.main = Prelude.putChar 'a'
