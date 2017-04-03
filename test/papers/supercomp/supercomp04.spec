module supercomp04 ( supercomp04.goal, supercomp04.main ) where

import Prelude

supercomp04.goal :: Prelude.Int -> Prelude.Int
supercomp04.goal v1 = supercomp04._pe0 v1

supercomp04.main :: Prelude.Int
supercomp04.main = supercomp04.goal (2 ? (3 ? 4))

supercomp04._pe0 :: Prelude.Int -> Prelude.Int
supercomp04._pe0 v1 = fcase v1 of
    3 -> 6
    4 -> 16
