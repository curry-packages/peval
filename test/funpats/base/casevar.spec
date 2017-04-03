module casevar ( casevar.null, casevar.goal, casevar.main ) where

import Prelude

casevar.null :: [a] -> Prelude.Bool
casevar.null v1 = fcase v1 of
    [] -> Prelude.True
    v2 : v3 -> Prelude.False

casevar.goal :: Prelude.Bool -> [a]
casevar.goal v1 = casevar._pe0 v1

casevar.main :: ([a],[b])
casevar.main = (casevar.goal Prelude.True,casevar.goal Prelude.False)

casevar._pe0 :: Prelude.Bool -> [a]
casevar._pe0 v1 = fcase v1 of
    Prelude.True -> []
    Prelude.False -> (let v2 free in v2) : (let v3 free in v3)
