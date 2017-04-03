module sqlast ( (sqlast.++), sqlast.goal, sqlast.main ) where

import Prelude

(sqlast.++) :: [a] -> [a] -> [a]
(sqlast.++) v1 v2 = fcase v1 of
    [] -> v2
    v3 : v4 -> v3 : (v4 sqlast.++ v2)

sqlast.goal :: [Prelude.Int] -> Prelude.Int
sqlast.goal v1 = sqlast._pe0 v1

sqlast.main :: Prelude.Int
sqlast.main = sqlast.goal (1 : (2 : (3 : [])))

sqlast._pe0 :: [Prelude.Int] -> Prelude.Int
sqlast._pe0 v1 = fcase v1 of
    v2 : v3 -> sqlast._pe1 v3 v2

sqlast._pe1 :: [Prelude.Int] -> Prelude.Int -> Prelude.Int
sqlast._pe1 v1 v2 = fcase v1 of
    [] -> v2 * v2
    v3 : v4 -> sqlast._pe1 v4 v3
