module xorSelf
  ( xorSelf.double, xorSelf.not, xorSelf.xorSelf, xorSelf.xor, xorSelf.main )
  where

import Prelude

xorSelf.double :: Prelude.Int -> Prelude.Int
xorSelf.double v1 = v1 + v1

xorSelf.not :: Prelude.Bool -> Prelude.Bool
xorSelf.not v1 = fcase v1 of
    Prelude.True -> Prelude.False
    Prelude.False -> Prelude.True

xorSelf.xorSelf :: Prelude.Bool -> Prelude.Bool
xorSelf.xorSelf v1 = xorSelf.xor v1 v1

xorSelf.xor :: Prelude.Bool -> Prelude.Bool -> Prelude.Bool
xorSelf.xor v1 v2 = fcase v1 of
    Prelude.True -> xorSelf.not v2
    Prelude.False -> v2

xorSelf.main :: Prelude.Bool -> Prelude.Bool
xorSelf.main v1 = xorSelf._pe0 v1

xorSelf._pe0 :: Prelude.Bool -> Prelude.Bool
xorSelf._pe0 v1 = fcase (case v1 of
      Prelude.True -> Prelude.True ? Prelude.False) of
    Prelude.True -> Prelude.False
    Prelude.False -> Prelude.False
