module base13 ( (base13.++), base13.nonLinear, base13.main ) where

import Prelude

(base13.++) :: [a] -> [a] -> [a]
(base13.++) v1 v2 = fcase v1 of
    [] -> v2
    v3 : v4 -> v3 : (v4 base13.++ v2)

base13.nonLinear :: [a] -> Prelude.Int
base13.nonLinear v1 = base13._pe0 v1

base13.main :: Prelude.Int
base13.main = (base13.nonLinear [])
  ?
  ((base13.nonLinear (1 : [])) ? (base13.nonLinear (1 : (1 : []))))

base13._pe0 :: [a] -> Prelude.Int
base13._pe0 v1 = fcase v1 of
    [] -> 42
    v2 : v3 -> fcase v3 of
        v4 : v5 -> (fcase v5 of
            [] -> (v2 Prelude.=:= v4) &> 42) ? ((let v6 free
          in (v6 Prelude.=:<= v4)
          &
          ((let v7,v8 free
          in (v7 Prelude.=:<= v2) &> (v8 base13.++ (v7 : (v6 : v8))))
          Prelude.=:<=
          v5))
          &>
          42)
