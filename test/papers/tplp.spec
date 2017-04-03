module tplp ( tplp.N (..), tplp.PEVAL, tplp.f, tplp.g, tplp.h, tplp.main ) where

import Prelude

data tplp.N
  = tplp.O
  | tplp.S tplp.N

tplp.PEVAL :: a -> a
tplp.PEVAL v1 = v1

tplp.f :: tplp.N -> tplp.N
tplp.f v1 = fcase v1 of
    tplp.O -> tplp.O

tplp.g :: tplp.N -> tplp.N
tplp.g v1 = tplp.S (tplp.f v1)

tplp.h :: tplp.N -> tplp.N
tplp.h v1 = fcase v1 of
    tplp.S v2 -> tplp.S tplp.O

tplp.main :: tplp.N
tplp.main = tplp.h tplp._pe0

tplp._pe0 :: tplp.N
tplp._pe0 = tplp.S Prelude.failed
