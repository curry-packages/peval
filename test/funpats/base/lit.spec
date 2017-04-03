module lit ( lit.id, lit.goal, lit.main ) where

import Prelude

lit.id :: a -> a
lit.id v1 = v1

lit.goal :: Prelude.Int -> Prelude.Int
lit.goal v1 = lit._pe0 v1

lit.main :: Prelude.Int
lit.main = (lit.goal 0) ? (lit.goal 1)

lit._pe0 :: Prelude.Int -> Prelude.Int
lit._pe0 v1 = fcase v1 of
    0 -> 1
