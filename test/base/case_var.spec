module case_var ( case_var.PEVAL, case_var.goal, case_var.main ) where

import Prelude

case_var.PEVAL :: a -> a
case_var.PEVAL v1 = v1

case_var.goal :: Prelude.Bool -> Prelude.Bool
case_var.goal v1 = case_var._pe0 v1

case_var.main :: (Prelude.Bool,Prelude.Bool)
case_var.main = (case_var.goal Prelude.True,case_var.goal Prelude.False)

case_var._pe0 :: Prelude.Bool -> Prelude.Bool
case_var._pe0 v1 = case v1 of
    Prelude.True -> Prelude.True
    Prelude.False -> Prelude.False
