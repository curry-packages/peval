module uni20 ( uni20.goal, uni20.main ) where

import Prelude

uni20.goal :: [a] -> Prelude.Bool
uni20.goal v1 = uni20._pe0 v1

uni20.main :: Prelude.Bool
uni20.main = (uni20.goal (1 : [])) ? (uni20.goal (1 : (2 : [])))

uni20._pe0 :: [a] -> Prelude.Bool
uni20._pe0 v1 = fcase v1 of
    v2 : v3 -> fcase v3 of
        [] -> v2 Prelude.=:= v2
