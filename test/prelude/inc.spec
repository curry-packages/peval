module inc ( inc.PEVAL, inc.inc, inc.main ) where

import Prelude

inc.PEVAL :: a -> a
inc.PEVAL v1 = v1

inc.inc :: Prelude.Int -> Prelude.Int
inc.inc v1 = v1 + 1

inc.main :: Prelude.Int
inc.main = inc._pe0

inc._pe0 :: Prelude.Int
inc._pe0 = 2
