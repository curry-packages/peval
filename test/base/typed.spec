module typed ( typed.PEVAL, typed.goal, typed.main ) where

import Prelude

typed.PEVAL :: a -> a
typed.PEVAL v1 = v1

typed.goal :: Prelude.Bool -> Prelude.Bool
typed.goal v1 = typed._pe0 v1

typed.main :: (Prelude.Bool,Prelude.Bool)
typed.main = (typed.goal Prelude.True,typed.goal Prelude.False)

typed._pe0 :: a -> a
typed._pe0 v1 = v1
