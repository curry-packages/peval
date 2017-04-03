module consnested ( consnested.id, consnested.goal, consnested.main ) where

import Prelude

consnested.id :: a -> a
consnested.id v1 = v1

consnested.goal :: [Prelude.Bool] -> Prelude.Int
consnested.goal v1 = consnested._pe0 v1

consnested.main :: Prelude.Int
consnested.main = (consnested.goal (Prelude.True : []))
  ?
  ((consnested.goal []) ? (consnested.goal (Prelude.False : [])))

consnested._pe0 :: [Prelude.Bool] -> Prelude.Int
consnested._pe0 v1 = fcase v1 of
    v2 : v3 -> fcase v2 of
        Prelude.True -> fcase v3 of
            [] -> 42
