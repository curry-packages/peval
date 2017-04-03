module supercomp07 ( supercomp07.goal, supercomp07.main ) where

import Prelude

supercomp07.goal :: Prelude.Bool -> (Prelude.Int,Prelude.Bool)
supercomp07.goal v1 = supercomp07._pe0 v1

supercomp07.main :: (Prelude.Int,Prelude.Bool)
supercomp07.main = (supercomp07.goal Prelude.True)
  ?
  (supercomp07.goal Prelude.False)

supercomp07._pe0 :: Prelude.Bool -> (Prelude.Int,Prelude.Bool)
supercomp07._pe0 v1 = case (case v1 of
      Prelude.True -> Prelude.False) of
    Prelude.False -> (2,Prelude.False)
