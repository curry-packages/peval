module justcoin
  ( justcoin.PEVAL, justcoin.coin, justcoin.just, justcoin.main )
  where

import Prelude

justcoin.PEVAL :: a -> a
justcoin.PEVAL v1 = v1

justcoin.coin :: Prelude.Int
justcoin.coin = 0 ? 1

justcoin.just :: a -> Prelude.Maybe a
justcoin.just v1 = Prelude.Just v1

justcoin.main :: Prelude.Maybe Prelude.Int
justcoin.main = justcoin._pe0

justcoin._pe0 :: Prelude.Maybe Prelude.Int
justcoin._pe0 = Prelude.Just (0 ? 1)
