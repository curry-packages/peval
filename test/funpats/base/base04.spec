module base04 ( (base04.++), base04.twoFP, base04.main ) where

import Prelude

(base04.++) :: [a] -> [a] -> [a]
(base04.++) v1 v2 = fcase v1 of
    [] -> v2
    v3 : v4 -> v3 : (v4 base04.++ v2)

base04.twoFP :: [a] -> [b] -> Prelude.Int
base04.twoFP v1 v2 = base04._pe0 v1 v2

base04.main :: Prelude.Int
base04.main = (base04.twoFP [] []) ? (base04.twoFP (1 : []) (1 : []))

base04._pe0 :: [a] -> [b] -> Prelude.Int
base04._pe0 v1 v2 = fcase v1 of
    [] -> fcase v2 of
        [] -> 42
