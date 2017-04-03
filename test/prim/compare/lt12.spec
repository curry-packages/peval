module lt12 ( lt12.goal, lt12.main ) where

import Prelude

lt12.goal :: Prelude.Bool -> Prelude.Bool
lt12.goal v1 = lt12._pe0 v1

lt12.main :: (Prelude.Bool,Prelude.Bool)
lt12.main = (lt12.goal Prelude.True,lt12.goal Prelude.False)

lt12._pe0 :: Prelude.Bool -> Prelude.Bool
lt12._pe0 v1 = case v1 of
    Prelude.True -> Prelude.False
    Prelude.False -> Prelude.True
