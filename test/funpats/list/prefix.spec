module prefix ( (prefix.++), prefix.goal, prefix.main ) where

import Prelude

(prefix.++) :: [a] -> [a] -> [a]
(prefix.++) v1 v2 = fcase v1 of
    [] -> v2
    v3 : v4 -> v3 : (v4 prefix.++ v2)

prefix.goal :: [a] -> [a]
prefix.goal v1 = prefix._pe0 v1

prefix.main :: [Prelude.Int]
prefix.main = prefix.goal (1 : (2 : (3 : [])))

prefix._pe0 :: [a] -> [a]
prefix._pe0 v1 = [] ? (fcase v1 of
    v2 : v3 -> (v2 : []) ? (fcase v3 of
        v4 : v5 -> let v6,v7 free
          in fcase v6 of
            [] -> (v7 Prelude.=:<= v4) &> (v2 : (v7 : []))
            v8 : v9 -> fcase v5 of
                v10 : v11 -> ((v7 Prelude.=:<= v4)
                  &
                  (prefix._pe1 v8 v10 v9 v11))
                  &>
                  (v2 : (v7 : (v8 : v9)))))

prefix._pe1 :: a -> a -> [a] -> [a] -> Prelude.Bool
prefix._pe1 v1 v2 v3 v4 = fcase v3 of
    [] -> v1 Prelude.=:<= v2
    v5 : v6 -> fcase v4 of
        v7 : v8 -> (v1 Prelude.=:<= v2) & (prefix._pe1 v5 v7 v6 v8)
