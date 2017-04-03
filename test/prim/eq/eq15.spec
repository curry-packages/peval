module eq15 ( eq15.PEVAL, eq15.goal, eq15.main ) where

import Prelude

eq15.PEVAL :: a -> a
eq15.PEVAL v1 = v1

eq15.goal :: Prelude.Int -> Prelude.Bool
eq15.goal v1 = v1 == 1

eq15.main :: (Prelude.Bool,Prelude.Bool)
eq15.main = (eq15.goal 0,eq15.goal 1)
