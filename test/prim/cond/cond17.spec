module cond17 ( cond17.PEVAL, cond17.goal, cond17.main ) where

import Prelude

cond17.PEVAL :: a -> a
cond17.PEVAL v1 = v1

cond17.goal :: Prelude.Bool -> a
cond17.goal v1 = cond17._pe0 v1

cond17.main :: a
cond17.main = (cond17.goal Prelude.True) ? (cond17.goal Prelude.False)

cond17._pe0 :: a -> b
cond17._pe0 v1 = Prelude.failed
