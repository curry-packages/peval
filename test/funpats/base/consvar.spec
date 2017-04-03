module consvar ( consvar.id, consvar.goal, consvar.main ) where

import Prelude

consvar.id :: a -> a
consvar.id v1 = v1

consvar.goal :: [a] -> a
consvar.goal v1 = consvar._pe0 v1

consvar.main :: Prelude.Bool
consvar.main = (consvar.goal (Prelude.True : []))
  ?
  ((consvar.goal []) ? (consvar.goal (Prelude.False : [])))

consvar._pe0 :: [a] -> a
consvar._pe0 v1 = fcase v1 of
    v2 : v3 -> fcase v3 of
        [] -> v2
