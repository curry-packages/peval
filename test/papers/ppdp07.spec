module ppdp07
  ( ppdp07.PEVAL, ppdp07.coin, ppdp07.repeat, ppdp07.heads, ppdp07.main )
  where

import Prelude

ppdp07.PEVAL :: a -> a
ppdp07.PEVAL v1 = v1

ppdp07.coin :: Prelude.Int
ppdp07.coin = 0 ? 1

ppdp07.repeat :: a -> [a]
ppdp07.repeat v1 = v1 : (ppdp07.repeat v1)

ppdp07.heads :: [a] -> (a,a)
ppdp07.heads v1 = fcase v1 of
    v2 : v3 -> fcase v3 of
        v4 : v5 -> (v2,v4)

ppdp07.main :: (Prelude.Int,Prelude.Int)
ppdp07.main = ppdp07._pe0

ppdp07._pe0 :: (Prelude.Int,Prelude.Int)
ppdp07._pe0 = let { v1 = 0 ? 1 } in (v1,v1)
