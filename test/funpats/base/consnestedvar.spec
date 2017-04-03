module consnestedvar
  ( consnestedvar.id, consnestedvar.goal, consnestedvar.main )
  where

import Prelude

consnestedvar.id :: a -> a
consnestedvar.id v1 = v1

consnestedvar.goal :: [Prelude.Bool] -> Prelude.Bool
consnestedvar.goal v1 = consnestedvar._pe0 v1

consnestedvar.main :: Prelude.Bool
consnestedvar.main = (consnestedvar.goal (Prelude.True : (Prelude.True : [])))
  ?
  ((consnestedvar.goal [])
  ?
  (consnestedvar.goal (Prelude.False : (Prelude.True : []))))

consnestedvar._pe0 :: [Prelude.Bool] -> Prelude.Bool
consnestedvar._pe0 v1 = fcase v1 of
    v2 : v3 -> fcase v3 of
        v4 : v5 -> fcase v4 of
            Prelude.True -> fcase v5 of
                [] -> v2
