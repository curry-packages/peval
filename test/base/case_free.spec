module case_free ( case_free.PEVAL, case_free.main ) where

import Prelude

case_free.PEVAL :: a -> a
case_free.PEVAL v1 = v1

case_free.main :: Prelude.Int
case_free.main = case (let v1 free in v1) of
    Prelude.True -> 42
