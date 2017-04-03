module cons ( cons.id, cons.goal, cons.main ) where

import Prelude

cons.id :: a -> a
cons.id v1 = v1

cons.goal :: Prelude.Bool -> Prelude.Int
cons.goal v1 = cons._pe0 v1

cons.main :: Prelude.Int
cons.main = (cons.goal Prelude.True) ? (cons.goal Prelude.False)

cons._pe0 :: Prelude.Bool -> Prelude.Int
cons._pe0 v1 = fcase v1 of
    Prelude.True -> 42
