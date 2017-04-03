module rcasevar ( rcasevar.rnull, rcasevar.goal, rcasevar.main ) where

import Prelude

rcasevar.rnull :: [a] -> Prelude.Bool
rcasevar.rnull v1 = case v1 of
    [] -> Prelude.True
    v2 : v3 -> Prelude.False

rcasevar.goal :: Prelude.Bool -> [a]
rcasevar.goal v1 = rcasevar._pe0 v1

rcasevar.main :: ([a],[b])
rcasevar.main = (rcasevar.goal Prelude.True,rcasevar.goal Prelude.False)

rcasevar._pe0 :: Prelude.Bool -> [a]
rcasevar._pe0 v1 = case (let v2 free in v2) of
    [] -> fcase v1 of
        Prelude.True -> []
    v3 : v4 -> fcase v1 of
        Prelude.False -> v3 : v4
