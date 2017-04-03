module cond13 ( cond13.PEVAL, cond13.goal, cond13.main ) where

import Prelude

cond13.PEVAL :: a -> a
cond13.PEVAL v1 = v1

cond13.goal :: Prelude.Bool -> Prelude.Bool
cond13.goal v1 = v1 &> Prelude.success

cond13.main :: Prelude.Bool
cond13.main = cond13.goal Prelude.success
