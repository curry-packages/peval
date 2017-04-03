module coin ( coin.PEVAL, coin.coin, coin.main ) where

import Prelude

coin.PEVAL :: a -> a
coin.PEVAL v1 = v1

coin.coin :: Prelude.Int
coin.coin = 0 ? 1

coin.main :: Prelude.Int
coin.main = coin.coin
