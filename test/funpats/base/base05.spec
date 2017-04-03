module base05 ( (base05.++), base05.sndFP, base05.main ) where

import Prelude

(base05.++) :: [a] -> [a] -> [a]
(base05.++) v1 v2 = fcase v1 of
    [] -> v2
    v3 : v4 -> v3 : (v4 base05.++ v2)

base05.sndFP :: [a] -> [b] -> Prelude.Int
base05.sndFP v1 v2 = fcase v1 of
    [] -> base05._pe0 v2

base05.main :: Prelude.Int
base05.main = (base05.sndFP [] []) ? (base05.sndFP [] (1 : []))

base05._pe0 :: [a] -> Prelude.Int
base05._pe0 v1 = fcase v1 of
    [] -> 42
