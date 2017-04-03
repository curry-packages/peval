module xorSelfCoin
  ( xorSelfCoin.not, xorSelfCoin.xor, xorSelfCoin.xorSelf, xorSelfCoin.coin
  , xorSelfCoin.main )
  where

import Prelude

xorSelfCoin.not :: Prelude.Bool -> Prelude.Bool
xorSelfCoin.not v1 = fcase v1 of
    Prelude.True -> Prelude.False
    Prelude.False -> Prelude.True

xorSelfCoin.xor :: Prelude.Bool -> Prelude.Bool -> Prelude.Bool
xorSelfCoin.xor v1 v2 = fcase v1 of
    Prelude.True -> xorSelfCoin.not v2
    Prelude.False -> v2

xorSelfCoin.xorSelf :: Prelude.Bool -> Prelude.Bool
xorSelfCoin.xorSelf v1 = xorSelfCoin.xor v1 v1

xorSelfCoin.coin :: Prelude.Bool
xorSelfCoin.coin = Prelude.False ? Prelude.True

xorSelfCoin.main :: Prelude.Bool
xorSelfCoin.main = xorSelfCoin._pe0

xorSelfCoin._pe0 :: Prelude.Bool
xorSelfCoin._pe0 = Prelude.False ? Prelude.False
