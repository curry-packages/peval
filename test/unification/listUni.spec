module listUni
  ( (listUni.=:=), listUni.check, listUni.goal, listUni.main )
  where

import Prelude

(listUni.=:=) :: [()] -> [()] -> Prelude.Bool
(listUni.=:=) v1 v2 = fcase v1 of
    [] -> fcase v2 of
        [] -> Prelude.success
    v3 : v4 -> fcase v3 of
        () -> fcase v2 of
            v5 : v6 -> fcase v5 of
                () -> v4 listUni.=:= v6

listUni.check :: [()] -> [()]
listUni.check v1 = let v2 free
  in case (v2 listUni.=:= v1) of
    Prelude.True -> v2
    Prelude.False -> Prelude.failed

listUni.goal :: [()] -> [()]
listUni.goal v1 = listUni._pe0 v1

listUni.main :: [()]
listUni.main = listUni.goal (Prelude.replicate 3 ())

listUni._pe0 :: [()] -> [()]
listUni._pe0 v1 = fcase v1 of
    [] -> []
    v2 : v3 -> fcase v2 of
        () -> let v4 free
          in fcase v4 of
            [] -> fcase v3 of
                [] -> () : []
            v5 : v6 -> fcase v5 of
                () -> fcase v3 of
                    v7 : v8 -> fcase v7 of
                        () -> listUni._pe1 v6 v8 (() : v6)

listUni._pe1 :: [()] -> [()] -> [()] -> [()]
listUni._pe1 v1 v2 v3 = fcase v1 of
    [] -> fcase v2 of
        [] -> () : v3
    v4 : v5 -> fcase v4 of
        () -> fcase v2 of
            v6 : v7 -> fcase v6 of
                () -> listUni._pe1 v5 v7 v3
