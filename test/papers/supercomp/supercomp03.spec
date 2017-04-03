module supercomp03 ( supercomp03.goal, supercomp03.main ) where

import Prelude

supercomp03.goal :: Prelude.Bool -> Prelude.Int
supercomp03.goal v1 = supercomp03._pe0 v1

supercomp03.main :: Prelude.Int
supercomp03.main = supercomp03.goal (Prelude.True ? Prelude.False)

supercomp03._pe0 :: Prelude.Bool -> Prelude.Int
supercomp03._pe0 v1 = case v1 of
    Prelude.True -> 4
    Prelude.False -> 5
