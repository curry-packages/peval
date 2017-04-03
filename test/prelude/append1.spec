module append1 ( append1.PEVAL, (append1.++), append1.main ) where

import Prelude

append1.PEVAL :: a -> a
append1.PEVAL v1 = v1

(append1.++) :: [a] -> [a] -> [a]
(append1.++) v1 v2 = fcase v1 of
    [] -> v2
    v3 : v4 -> v3 : (v4 append1.++ v2)

append1.main :: [a]
append1.main = append1._pe0

append1._pe0 :: a
append1._pe0 = let v1 free in v1
