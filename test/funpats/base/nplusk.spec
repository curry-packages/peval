module nplusk ( nplusk.prec, nplusk.main ) where

import Prelude

nplusk.prec :: Prelude.Int -> Prelude.Int
nplusk.prec v1 = nplusk._pe0 v1

nplusk.main :: Prelude.Int
nplusk.main = nplusk.prec 1

nplusk._pe0 :: Prelude.Int -> Prelude.Int
nplusk._pe0 v1 = let v2 free in ((v2 + 1) Prelude.=:<= v1) &> v2
