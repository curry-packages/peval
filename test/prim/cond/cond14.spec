module cond14 ( cond14.PEVAL, cond14.goal, cond14.main ) where

import Prelude

cond14.PEVAL :: a -> a
cond14.PEVAL v1 = v1

cond14.goal :: Prelude.Bool -> a
cond14.goal v1 = v1 &> Prelude.failed

cond14.main :: a
cond14.main = cond14.goal Prelude.success
