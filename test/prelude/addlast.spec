module addlast ( addlast.PEVAL, (addlast.++), addlast.goal, addlast.main ) where

import Prelude

addlast.PEVAL :: a -> a
addlast.PEVAL v1 = v1

(addlast.++) :: [a] -> [a] -> [a]
(addlast.++) v1 v2 = fcase v1 of
    [] -> v2
    v3 : v4 -> v3 : (v4 addlast.++ v2)

addlast.goal :: [a]
addlast.goal = addlast._pe0

addlast.main :: Prelude.Bool
addlast.main = Prelude.null addlast.goal

addlast._pe0 :: [a]
addlast._pe0 = ((let v1 free in v1) : []) ? ((let v2 free in v2) : addlast._pe0)
