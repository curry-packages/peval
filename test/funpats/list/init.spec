module init ( init.PEVAL, (init.++), init.init, init.main ) where

import Prelude

init.PEVAL :: a -> a
init.PEVAL v1 = v1

(init.++) :: [a] -> [a] -> [a]
(init.++) v1 v2 = fcase v1 of
    [] -> v2
    v3 : v4 -> v3 : (v4 init.++ v2)

init.init :: [a] -> [a]
init.init v1 = init._pe0 v1

init.main :: [Prelude.Int]
init.main = init.init (1 : (2 : (3 : [])))

init._pe0 :: [a] -> [a]
init._pe0 v1 = fcase v1 of
    v2 : v3 -> fcase v3 of
        [] -> []
        v4 : v5 -> fcase v5 of
            [] -> v2 : []
            v6 : v7 -> (fcase v7 of
                [] -> v2 : (v4 : [])) ? (let v8,v9 free
              in fcase v8 of
                [] -> fcase v7 of
                    v10 : v11 -> fcase v11 of
                        [] -> (v9 Prelude.=:<= v6) &> (v2 : (v4 : (v9 : [])))
                v12 : v13 -> fcase v7 of
                    v14 : v15 -> ((v9 Prelude.=:<= v6)
                      &
                      (init._pe1 v12 v14 v13 v15))
                      &>
                      (v2 : (v4 : (v9 : (v12 : v13)))))

init._pe1 :: a -> a -> [a] -> [a] -> Prelude.Bool
init._pe1 v1 v2 v3 v4 = fcase v3 of
    [] -> fcase v4 of
        v5 : v6 -> fcase v6 of
            [] -> v1 Prelude.=:<= v2
    v7 : v8 -> fcase v4 of
        v9 : v10 -> (v1 Prelude.=:<= v2) & (init._pe1 v7 v9 v8 v10)
