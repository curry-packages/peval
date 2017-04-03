module double_coin
  ( double_coin.PEVAL, double_coin.coin, double_coin.double, double_coin.main )
  where

import Prelude

double_coin.PEVAL :: a -> a
double_coin.PEVAL v1 = v1

double_coin.coin :: Prelude.Int
double_coin.coin = 0 ? 1

double_coin.double :: Prelude.Int -> Prelude.Int
double_coin.double v1 = v1 + v1

double_coin.main :: Prelude.Int
double_coin.main = double_coin._pe0

double_coin._pe0 :: Prelude.Int
double_coin._pe0 = 0 ? 2
