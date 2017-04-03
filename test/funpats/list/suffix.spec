module suffix ( (suffix.++), suffix.goal, suffix.main ) where

import Prelude

(suffix.++) :: [a] -> [a] -> [a]
(suffix.++) v1 v2 = fcase v1 of
    [] -> v2
    v3 : v4 -> v3 : (v4 suffix.++ v2)

suffix.goal :: [a] -> [a]
suffix.goal v1 = suffix._pe0 v1

suffix.main :: [Prelude.Int]
suffix.main = suffix.goal (1 : (2 : (3 : [])))

suffix._pe0 :: [a] -> [a]
suffix._pe0 v1 = v1 ? (fcase v1 of
    v2 : v3 -> suffix._pe1 v2 v3)

suffix._pe1 :: a -> [a] -> [a]
suffix._pe1 v1 v2 = v2 ? (fcase v2 of
    v3 : v4 -> suffix._pe1 v3 v4)
