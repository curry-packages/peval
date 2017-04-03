module base03 ( (base03.++), base03.unaryGuard, base03.main ) where

import Prelude

(base03.++) :: [a] -> [a] -> [a]
(base03.++) v1 v2 = fcase v1 of
    [] -> v2
    v3 : v4 -> v3 : (v4 base03.++ v2)

base03.unaryGuard :: [a] -> Prelude.Int
base03.unaryGuard v1 = base03._pe0 v1

base03.main :: Prelude.Int
base03.main = (base03.unaryGuard []) ? (base03.unaryGuard (1 : []))

base03._pe0 :: [a] -> Prelude.Int
base03._pe0 v1 = fcase v1 of
    [] -> 42
