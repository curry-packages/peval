module let_coin ( let_coin.PEVAL, let_coin.coin, let_coin.main ) where

import Prelude

let_coin.PEVAL :: a -> a
let_coin.PEVAL v1 = v1

let_coin.coin :: Prelude.Int
let_coin.coin = 0 ? 1

let_coin.main :: (Prelude.Int,Prelude.Int)
let_coin.main = let { v1 = let_coin.coin } in (v1,v1)
