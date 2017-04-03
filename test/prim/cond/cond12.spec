module cond12 ( cond12.PEVAL, cond12.goal, cond12.main ) where

import Prelude

cond12.PEVAL :: a -> a
cond12.PEVAL v1 = v1

cond12.goal :: a -> a
cond12.goal v1 = cond12._pe0 v1

cond12.main :: Prelude.Int
cond12.main = cond12.goal 1

cond12._pe0 :: a -> b
cond12._pe0 v1 = Prelude.failed
