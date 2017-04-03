module index ( (index.=:=), index.goal, index.main ) where

import Prelude

(index.=:=) :: [()] -> [()] -> Prelude.Bool
(index.=:=) v1 v2 = fcase v1 of
    [] -> fcase v2 of
        [] -> Prelude.success
    v3 : v4 -> fcase v3 of
        () -> fcase v2 of
            v5 : v6 -> fcase v5 of
                () -> v4 index.=:= v6

index.goal :: [()] -> a -> a
index.goal v1 v2 = index._pe0 v1 v2

index.main :: Prelude.Bool
index.main = index.goal (Prelude.replicate 3 ()) Prelude.True

index._pe0 :: [()] -> a -> a
index._pe0 v1 v2 = fcase v1 of
    [] -> v2
    v3 : v4 -> fcase v3 of
        () -> index._pe0 v4 v2
