module xor_case ( xor_case.xor, xor_case.goal, xor_case.main ) where

import Prelude

xor_case.xor :: Prelude.Bool -> Prelude.Bool -> Prelude.Bool
xor_case.xor v1 v2 = fcase v1 of
    Prelude.False -> fcase v2 of
        Prelude.False -> Prelude.True
        Prelude.True -> Prelude.False
    Prelude.True -> fcase v2 of
        Prelude.False -> Prelude.False
        Prelude.True -> Prelude.True

xor_case.goal :: () -> Prelude.Bool
xor_case.goal v1 = xor_case._pe0 v1

xor_case.main :: Prelude.Bool
xor_case.main = xor_case.goal ()

xor_case._pe0 :: () -> Prelude.Bool
xor_case._pe0 v1 = fcase (case v1 of
      () -> Prelude.False ? Prelude.True) of
    Prelude.False -> Prelude.True
    Prelude.True -> Prelude.True
