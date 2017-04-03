module conslit ( conslit.id, conslit.goal, conslit.main ) where

import Prelude

conslit.id :: a -> a
conslit.id v1 = v1

conslit.goal :: [Prelude.Int] -> Prelude.Int
conslit.goal v1 = conslit._pe0 v1

conslit.main :: Prelude.Int
conslit.main = (conslit.goal (0 : []))
  ?
  ((conslit.goal []) ? (conslit.goal (1 : [])))

conslit._pe0 :: [Prelude.Int] -> Prelude.Int
conslit._pe0 v1 = fcase v1 of
    v2 : v3 -> fcase v2 of
        0 -> fcase v3 of
            [] -> 42
