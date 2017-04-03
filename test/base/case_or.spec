module case_or ( case_or.PEVAL, (case_or.?), case_or.main ) where

import Prelude

case_or.PEVAL :: a -> a
case_or.PEVAL v1 = v1

(case_or.?) :: a -> a -> a
(case_or.?) v1 v2 = v1 ? v2

case_or.main :: Prelude.Int
case_or.main = case_or._pe0

case_or._pe0 :: Prelude.Int
case_or._pe0 = 42
