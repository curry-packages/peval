module uni14 ( uni14.PEVAL, uni14.goal, uni14.main ) where

import Prelude

uni14.PEVAL :: a -> a
uni14.PEVAL v1 = v1

uni14.goal :: Prelude.Bool -> Prelude.Bool
uni14.goal v1 = uni14._pe0 v1

uni14.main :: Prelude.Bool
uni14.main = uni14.goal (Prelude.True ? Prelude.False)

uni14._pe0 :: Prelude.Bool -> Prelude.Bool
uni14._pe0 v1 = case v1 of
    Prelude.True -> Prelude.True
