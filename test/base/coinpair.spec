module coinpair
  ( coinpair.PEVAL, coinpair.coin, coinpair.pair, coinpair.main )
  where

import Prelude

coinpair.PEVAL :: a -> a
coinpair.PEVAL v1 = v1

coinpair.coin :: Prelude.Int
coinpair.coin = 0 ? 1

coinpair.pair :: a -> (a,a)
coinpair.pair v1 = (v1,v1)

coinpair.main :: (Prelude.Int,Prelude.Int)
coinpair.main = coinpair._pe0

coinpair._pe0 :: (Prelude.Int,Prelude.Int)
coinpair._pe0 = let { v1 = 0 ? 1 } in (v1,v1)
